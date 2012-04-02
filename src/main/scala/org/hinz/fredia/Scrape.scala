package org.hinz.freida

import scala.collection.JavaConversions._
import scala.util.matching.Regex

import org.hinz.base._
import org.hinz.validation._

import scala.xml._

import dispatch._
import dispatch.Request._
import dispatch.jsoup.JSoupHttp._

object log {
  def apply[T:Showable](a: T) = { show(a); a }
}

class Freida {
  // Base Request addresses
  val baseReq = :/("freida.ama-assn.org") / "Freida"
  val userReq = baseReq / "user"

  // Search page [note- post to this page when searching]
  val progSearch = userReq / "viewProgramSearch.do" secure

  // EULA
  val eulaSubmit = baseReq / "eulaSubmit.do" secure

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

    val stateSearchOK = postStateSearch(state, http)

    val specSearchOK =
      stateSearchOK.flatMap(
        Unit => postSpecSearch(program, http))

    val idPattern = new Regex("pgmNumber=(\\d+)")

    val finalOK = specSearchOK.flatMap { 
      Unit => {
        Valid(http(resultsList << Map("searchImageButton" -> "Search", "sort" -> "spec") >- (str =>
          { 
            str.split("\n").flatMap (str=>
                                     idPattern.findFirstMatchIn(str)) map (_.group(1)) toList
          })))
      }
    }
             
    log(finalOK)
    finalOK
  }
}

object Test {


  def main(args: Array[String]) {
    val f = new Freida()
    //f.searchListSpecsWithOpenSession(f.httpWithSession)
    log(f.searchPrograms("Pennsylvania","Internal Medicine"))
  }
}
