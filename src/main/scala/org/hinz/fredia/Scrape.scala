package org.hinz.freida

import scala.collection.JavaConversions._
import scala.util.matching.Regex

import scala.annotation.tailrec

import org.hinz.base._
import org.hinz.validation._

import org.jsoup.nodes._

import dispatch._
import dispatch.Request._
import dispatch.jsoup.JSoupHttp._

object log {
  def apply[T:Showable](a: T) = { show(a); a }
}

class FreidaService {
  // Base Request addresses
  val baseReq = :/("freida.ama-assn.org") / "Freida"
  val userReq = baseReq / "user"

  // Search page [note- post to this page when searching]
  val progSearch = userReq / "viewProgramSearch.do" secure

  // EULA
  val eulaSubmit = baseReq / "eulaSubmit.do" secure

  // Print
  val pgmPrint = userReq / "pgmPrint.do" secure

  // Search specifications
  val searchList = userReq / "programSearchDispatch.do" secure
  val specSearchList = searchList <<? Map("method" -> "viewSpec")  secure
  val stateSearchList = searchList <<? Map("method" -> "viewStateLocation") secure

  // Search post params
  val searchStatePostMeta = Map("method" -> "continueWithSearch",
                                "fromPage" -> "viewPage")

  val searchSpecPostMeta = Map("method" -> "continueWithSearch",
                               "fromPage" -> "viewSpec")


  // Final Search
  val resultsList = userReq / "programSearchSubmitDispatch.do" secure

  def httpWithSession = {
    val http = new Http
    http(progSearch </> {
      doc =>
        if (doc.select("#eulaForm").size() > 0) {
          log("EULA Form - posting EULA request")
          http(eulaSubmit << Map("accept" -> "true") >|)
        }
    })

    http
  }

  type SearchResult = (Map[String,(String,String)], Map[String,String])

  def searchListWithOpenSession(req: Request, http:Http):SearchResult = {
    val handler = req </> { 
      doc => {
        val part1 = doc.select(".application-table").select("input").grouped(2).map(a => (a(0), a(1))).map {
          case (e1:org.jsoup.nodes.Element, e2:org.jsoup.nodes.Element) => e2.attr("value") -> (e1.attr("name"), e1.attr("value"))
        }.foldLeft(Map[String,(String,String)]())(_+_)

        val part2 = doc.select(".application-table").select("input").grouped(2).map(a => (a(0), a(1))).map {
          case (e1:org.jsoup.nodes.Element, e2:org.jsoup.nodes.Element) => e2.attr("name") -> e2.attr("value")
        }.foldLeft(Map[String,String]())(_+_)

        (part1, part2)
      }
    }

    http(handler)
  }

  def stateList():Set[String] = 
    searchListWithOpenSession(stateSearchList, httpWithSession)._1.keySet
    
  def postSearch(search: String , http: Http, req: Request, meta: Map[String,String], searchReq: Request):Validation[Unit] = {
    val (matches, extra) = searchListWithOpenSession(searchReq, http)
    val matchOpt = matches.get(search)

    matchOpt match {
      case Some((k,v)) => 
        http.x(req << (extra ++ Map(k -> v) ++ meta)) {
          case (200, _, _) => Valid(Unit):Validation[Unit]
          case (code,_, _) => Error(List(asString(code))):Validation[Unit]
        }
      case None => Error(List("Could not find %s in %s" format (asString(search), asString(matches))))
    }
  }

  def postStateSearch(state: String, http: Http):Validation[Unit] = 
    postSearch(state, http, searchList, searchStatePostMeta, stateSearchList)

  def postSpecSearch(spec: String, http: Http):Validation[Unit] = 
    postSearch(spec, http, searchList, searchSpecPostMeta, specSearchList)


  def searchPrograms(state: String, program: String):Validation[List[String]] = {
    // Flow:
    // 
    // Get a session
    // Grab list of states and intersect with "states"
    // POST state list
    // Grab a list of programs and intersect with "programs"
    // POST program list
    // POST search form
    // Parse results
    // Requires 8 hits
    val http = httpWithSession
    val idPattern = new Regex("pgmNumber=(\\d+)")

    postStateSearch(state, http) >>=
      (Unit => postSpecSearch(program, http)) >>=
        (Unit => {
          Valid(http(resultsList << Map("searchImageButton" -> "Search", "sort" -> "spec") >- (str =>
            { 
              str.split("\n").flatMap (str=>
                                       idPattern.findFirstMatchIn(str)) map (_.group(1)) toList
            })))})
  }

  def extractTableCellPattern(d: Document, regex: Regex):Option[String] = {
    d select("td") flatMap(s => regex.findFirstMatchIn(s.toString())) map (_.group(1)) match {
      case Seq() => None
      case Seq(x, _*) => Some(x)
    }
  }

  case class Program(lastUpdated: Option[String], surveyReceived: Option[String], director: Option[Contact], contact: Option[Contact], webAddr: String)
  case class Contact(name: String, address: String, contact: Map[String,String])

  def exhaustMatch(s: String, r: Regex):List[Regex.Match] = r.findFirstMatchIn(s) match {
    case Some(m) => m :: exhaustMatch(m.after(2).toString, r)
    case None => Nil
  }

  def extractInfoFromTable(t: Element):Option[Contact] = {
    val tds = t select("tr") apply(1) select("td") 
    if (tds.length == 3) {
      val addr = tds(0).toString
      val addrMatches = """>\s*([^<]+)<.*?>(.*)</td>""".r.findFirstMatchIn(addr)

      val contactRaw = tds(2).toString.replaceAll("&nbsp;"," ")
      val matches = exhaustMatch(contactRaw, """>\s+([^<^>]*?)\:.*?\s+([^<]*) <""".r).map(m => (m.group(1).trim, m.group(2).trim))

      Some(Contact(addrMatches.map(_.group(1)).getOrElse(""), 
                   addrMatches.map(_.group(2).replaceAll("<.*?>","").replaceAll("&nbsp;"," ")).getOrElse(""), 
                   matches.foldLeft(Map[String,String]())(_+_)))
    } else {
      None
    }
  }

  def extractProgramDirector(d: Document):Option[Contact] = {
    extractInfoFromTable(d.select("table")(3))
  }

  def extractProgramContact(d: Document):Option[Contact] = {
    extractInfoFromTable(d.select("table")(4))
  }

  def parseProgram(d: Document):Program = {

    // extract last updated and survey rcvd
    Program(
      extractTableCellPattern(d, """Last updated.*(\d+/\d+/\d+)<""".r),
      extractTableCellPattern(d, """Survey received.*(\d+/\d+/\d+)""".r),
      extractProgramDirector(d),
      extractProgramContact(d),
      d.select("a[target=FREIDAEXT").attr("href"))

  }

  def getProgram(pid: String, http: Http):Program = {
    http(searchList <<? Map("method" -> "searchByPgmNbr", "pgmNumber" -> pid, "page" -> "1") >|)
    http(pgmPrint </> (d => parseProgram(d)))
  }
}

object Freida {
  def insertProgramsForSpec(spec: String, f: FreidaService, m: FreidaDAO):Validation[Int] = {
    insertPrograms(f.stateList() toList, spec, f, m)
  }

  @tailrec
  def insertPrograms(states: List[String], spec: String, f: FreidaService, m: FreidaDAO, v: Validation[Int] = Valid(0)):Validation[Int] = states match {
    case s :: ss => {
      log("Inserting programs for %s in %s (%d to go)" format (spec, s, ss.length))
      Thread.sleep(2000)
      insertPrograms(ss, spec, f, m, v >>= (cnt => insertProgram(s, spec, f, m) map (_ + cnt)))
    }
    case Nil => v
  }
      

  // returns # of programs inserted
  def insertProgram(state: String, spec: String, f: FreidaService, m: FreidaDAO):Validation[Int] = {
    f.searchPrograms(state,spec) map
    (programs => (programs.map(pid => m.insertProgram(pid, state, spec)) map (if(_) 1 else 0)).foldLeft(0)(_+_))
  }
}


object Test {
  def main(args: Array[String]) {
    val f = new FreidaService()
    val fm = new FreidaDAO()
    // f.searchPrograms("Pennsylvania","Internal Medicine") >>=
    // (programs => programs.map(fm.insertProgram(
    // log(fm.insertProgram("1404121374","Pennsylvania","Internal Medicine"))
//    log(Freida.insertPrograms("Pennsylvania","Internal Medicine", f, fm))
//    log(f.stateList())
//    log(Freida.insertProgramsForSpec("Internal Medicine", f, fm))
    log(f.getProgram("1401021091", f.httpWithSession))
  }
}
