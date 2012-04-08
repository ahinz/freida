package org.hinz.freida

import org.hinz.freida.mongo._

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

case class Institution(val iid: String, name: String, address: String, clinicalEnv: Option[Map[String, String]], 
                       resources: Option[Map[String, (String,String)]], medicalSchools: Option[List[(String,String)]])

case class Program(programId: String,
                   lastUpdated: Option[String], 
                   surveyReceived: Option[String], 
                   director: Option[Contact],
                   contact: Option[Contact], 
                   webAddr: String,
                   basicInfo: Option[Map[String,String]],
                   institutions: Option[Map[String,List[String]]],
                   programSize: Option[Map[String,String]],
                   programInfo: Option[Map[String,String]],
                   usmleReqs: Option[Map[String,String]],
                   faculty: Option[Faculty],
                   hours: Option[Map[String,String]],
                   callSchedule: Option[Map[String,(String,String)]],
                   education: Option[Map[String,String]],
                   evals: Option[Map[String,String]],
                   benefits: Option[Map[String,String]],
                   salary: Option[Map[String,(String,String,String)]])

case class Faculty(fullTimePhy: Int, fullTimeNonPhy: Int, partTimePhy: Int, partTimeNonPhy: Int)
case class Contact(name: String, address: String, contact: Map[String,String])

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
  val instPrint = userReq / "instPrint.do" secure

  // Search specifications
  val searchList = userReq / "programSearchDispatch.do" secure
  val specSearchList = searchList <<? Map("method" -> "viewSpec")  secure
  val stateSearchList = searchList <<? Map("method" -> "viewStateLocation") secure
  val searchInstList = userReq / "institutionSearchSubmit.do" secure

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

  def exhaustMatch(s: String, r: Regex):List[Regex.Match] = r.findFirstMatchIn(s) match {
    case Some(m) => m :: exhaustMatch(m.after(2).toString, r)
    case None => Nil
  }

  def extractHours(e: Element):Option[Map[String,String]] =
    findNextTableWithText(e, "Work schedule") map extractTablePairs


  def extractContactInfoFromTable(t: Element):Option[Contact] = {
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

  def strip(s: String):String = s.replaceAll("\u00A0","").trim

  def extractTablePairs(t: Element):Map[String,String] = {
    t.select("tr").map(_.select("td")).filter(a => a.length == 2 && a(0).text.length > 0).map(a => strip(a(0).text) -> strip(a(1).text)).foldLeft(Map[String,String]())(_+_)
  }

  def extractFaculty(e: Element):Option[Faculty] = {
    findNextTableWithText(e, "Program Faculty") map (elmt => {
      val tds = elmt.select("td")
      Faculty(tds(4).text.toInt, tds(5).text.toInt, tds(7).text.toInt, tds(8).text.toInt)
    })
  }

  def extractUSMLEReqs(e: Element):Option[Map[String,String]] =
    findNextTableWithText(e, "USMLE Step 1 and Step 2 requirements") map (elmt => {
      val rows = elmt.select("tr")
      val keys = rows(0).select("td").map(_.text)
      val vals = rows(1).select("td").map(_.text)

      Map((keys zip vals):_*)
    })

  def findNextTableWithText(d: Element, t: String):Option[Element] =
    d.select("table").sliding(2).foldLeft(None:Option[Element]) {
      case (None, Seq(e,eNext)) => if (e.text.contains(t)) Some(eNext) else None
      case (s@Some(_), _) => s
    }

  def findTableWithText(d: Element, t: String):Option[Element] =
    d.select("table").foldLeft(None:Option[Element]) {
      case (None, e) => if (e.text.contains(t)) Some(e) else None
      case (s@Some(_), e) => s
    }

  def findTableWithText(d: Element, t: List[String]):Option[Element] = t match {
    case Nil => None
    case x :: xs => findTableWithText(d, x) match {
      case s@Some(tbl) => s
      case None => findTableWithText(d, xs)
    }
  }

  def extractProgramInfo(e: Element) =
    findTableWithText(e, "Interview") map extractTablePairs

  def extractProgramSize(e: Element):Option[Map[String,String]] =
    findNextTableWithText(e, "Total program size") map extractTablePairs map (_ - "Year")

  def extractProgramDirector(d: Document):Option[Contact] =
    findTableWithText(d, "Program Director:") flatMap extractContactInfoFromTable

  def extractProgramContact(d: Document):Option[Contact] = 
    findTableWithText(d, "Person to contact") flatMap extractContactInfoFromTable

  def extractCallSchedule(e: Element):Option[Map[String,(String,String)]] =
    findTableWithText(e, "Most taxing schedule") map (elmt => {
      elmt.select("tr").tail map (_.select("td")) map (e => e(0).text -> (e(1).text,e(2).text))
    }) map (e => Map(e:_*) )

  def extractEducation(e: Element):Option[Map[String,String]] = 
    findTableWithText(e, "Educational setting") map extractTablePairs map (_ - "Educational setting") flatMap ( m =>
      findTableWithText(e, "Educational benefits") map extractTablePairs map (z => z ++ m)) flatMap (m =>
        findTableWithText(e, "Educational features") map extractTablePairs map (z => z ++ m))

  def extractEvals(e: Element):Option[Map[String,String]] =
    findTableWithText(e, "Resident evaluation") map extractTablePairs flatMap ( m =>
      findTableWithText(e, "Program evaluation") map extractTablePairs map (z => z ++ m))

  def extractEmploymentBenefits(e: Element):Option[Map[String,String]] =
    findNextTableWithText(e, "Employment Policies and Benefits") map extractTablePairs flatMap (m =>
      findNextTableWithText(e, "Major medical benefits") map extractTablePairs)

  def extractSalary(e: Element) =
    findNextTableWithText(e, "Compensation and leave") map (elmt =>
      (elmt select("tr") tail) map (e => e.select("td") map (_.text)) map ( e => e(0) -> (e(1), e(2), e(3)))) map (e => Map(e:_*))

  def extractInsts(e: Element):Option[Map[String,List[String]]] = {
    val inst = "forward=(.*)&instNbr=(\\d+)".r
    
    findTableWithText(e, "Institution list").map(_.select("[href]").map(_.attr("href")).map(inst.findFirstMatchIn(_)).map(a => (a.get.group(1),a.get.group(2))).foldLeft(Map[String,List[String]]()) {
      case (m,(f,i)) => m + (f -> (i :: m.get(f).getOrElse(List())))
    })
  }

  val basicInfoStrs = List("Accredited length of training", "Required length", "Accepting applications", "Program start")

  def parseProgram(pId: String, d: Document):Program = {
    Program(
      pId,
      extractTableCellPattern(d, """Last updated.*(\d+/\d+/\d+)<""".r),
      extractTableCellPattern(d, """Survey received.*(\d+/\d+/\d+)""".r),
      extractProgramDirector(d),
      extractProgramContact(d),
      d.select("a[target=FREIDAEXT").attr("href"),
      findTableWithText(d, basicInfoStrs) map extractTablePairs,
      extractInsts(d),
      extractProgramSize(d),
      extractProgramInfo(d),
      extractUSMLEReqs(d),
      extractFaculty(d),
      extractHours(d),
      extractCallSchedule(d),
      extractEducation(d),
      extractEvals(d),
      extractEmploymentBenefits(d),
      extractSalary(d))

  }

  def elementToString(e: Element) = strip(e.text)

  def extractNameAndAddress(e: Element) = {
    val fonts = e.select("table")(0).select("font")
    (elementToString(fonts(0)), elementToString(fonts(1)))
  }

  def convertBeds(e: Element) = {
    val es = elementToString(e)
    if (es.indexOf("Beds") >= 0) {
      val s = es replace("Beds","")
      if (s.length == 0) "0" else s
    } else {
      es
    }
  }

  def extractResources(e: Element) =
    findNextTableWithText(e, "Special clinical resources") map (tbl => {
      tbl select("tr") map (_.select("td")) map (row => elementToString(row(0)) -> (elementToString(row(1)), convertBeds(row(2))))
    }) map (a => Map(a:_*))

  def extractMedAf(e: Element) =
    findTableWithText(e, "This institution has the following affiliations") map (tbl =>
      tbl select("tr") map (_.select("td")) filter(_.length == 3) map (a => (elementToString(a(0)), elementToString(a(1)))) toList)

  def parseInstitution(iId: String, d: Document):Institution = {
    val (name, addr) = extractNameAndAddress(d)
    Institution(iId, 
                name, 
                addr,
                findTableWithText(d, "Clinical environment") map extractTablePairs map (_.mapValues( a => if (a.length == 0) "0" else a )),
                extractResources(d),
                extractMedAf(d))
  }

  def getInstitution(iid: String, http: Http):Institution = {
    http(searchInstList <<? Map("forward" -> "affiliated", "instNbr" -> iid) >|)
    http(instPrint </> (i => parseInstitution(iid, i)))
  }

  def getProgram(pid: String, http: Http):Program = {
    http(searchList <<? Map("method" -> "searchByPgmNbr", "pgmNumber" -> pid, "page" -> "1") >|)
    http(pgmPrint </> (d => parseProgram(pid,d)))
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

  def insertProgramData(pIds: List[String], f: FreidaService, m: FreidaDAO) = {
    pIds map ( p => {
      Thread.sleep(500)
      log("Working on program %s" format p)
      m.updateProgram(p, asMongo(f.getProgram(p, f.httpWithSession)))
    })
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
//    log(f.getProgram("1401021091", f.httpWithSession))
//    log(fm.updateProgram("1401021091", asMongo(f.getProgram("1401021091", f.httpWithSession))))
//    log(f.getProgram("1401512544", f.httpWithSession))
//    log(fm.findProgram("1401021091"))
    // fm.programIds map ( p => {
    //   Thread.sleep(500)
    //   log("Working on program %s" format p)
    //   fm.updateProgram(p, asMongo(f.getProgram(p, f.httpWithSession)))
    // })

//    Println(asMongo(f.getProgram("1404111375", f.httpWithSession)))
//    log(f.getInstitution("410189", f.httpWithSession))
//    fm.insertInstitution("410189", asMongo(f.getInstitution("410189", f.httpWithSession)))
    log(fm.findInstitution("410189"))
    println("-end-")
  }
}
