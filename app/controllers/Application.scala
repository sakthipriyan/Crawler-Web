package controllers

import play.api._
import play.api.mvc._
import play.api.templates.Html
import play.Play
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.SortedMap
import play.api.libs.json._

case class RateIt(review: String, rating: String)

object Application extends Controller {

  val bad = getWords("bad")
  val average = getWords("average")
  val good = getWords("good")

  def index(review: Option[String]) = Action { implicit request =>
    review map { text =>
      {
        val rating = getRating(text)
        val rateit = RateIt(text, rating)
        render {
          case Accepts.Html() => Ok(views.html.main(Some(rateit)))
          case Accepts.Json() => Ok(Json.obj("review" -> rateit.review, "rating" -> rateit.rating))
        }
      }
    } getOrElse Ok(views.html.main())
  }

  private def getWords(rating: String) = {
    val output = new ArrayBuffer[String]
    val source = scala.io.Source.fromInputStream(Play.application.resourceAsStream(rating))
    for (line <- source.getLines)
      output += line.trim()
    source.close
    output.toSet
  }

  private def getRating(review: String): String = {
    val processed = review.replaceAll("\\<.*?>", "").replaceAll("[^a-zA-Z]", " ").replaceAll(" +", " ").trim().toLowerCase()
    val reviewSet = processed.split(" ").toSet
    val badCount = 1000 - (bad -- reviewSet).size
    val averageCount = 1000 - (average -- reviewSet).size
    val goodCount = 1000 - (good -- reviewSet).size
    val sortedList = List(("Bad", badCount), ("Average", averageCount), ("Good", goodCount)).sortBy(_._2)
    println(sortedList)
    if (sortedList(1)._2 == sortedList(2)._2 || sortedList(2)._2 == 0)
      "We don't know yet"
    else
      sortedList(2)._1
  }
}