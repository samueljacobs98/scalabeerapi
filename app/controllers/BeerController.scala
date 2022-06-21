package controllers

import models._
import play.api.mvc.{BaseController, ControllerComponents}
import play.api.libs.json._

import javax.inject._
import scala.collection.mutable

@Singleton
class BeerController @Inject()(val controllerComponents: ControllerComponents)
extends BaseController {
  private val beerList = new mutable.ListBuffer[BeerItem]
  beerList += BeerItem(1, "Corona", "Pretty nice, unfortunate name", 3.6)
  beerList += BeerItem(2, "San Miguel", "Spanish beer (i think).. hola", 4.5)

  implicit val beerListJson: OFormat[BeerItem] = Json.format[BeerItem]
  implicit val newBeerListJson: OFormat[NewBeerItem] = Json.format[NewBeerItem]

  def addNewBeer() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val newBeerItem: Option[NewBeerItem] = jsonObject.flatMap(Json.fromJson[NewBeerItem](_).asOpt)

    newBeerItem match {
      case Some(newBeer) =>
        val nextId = beerList.map(_.id).max + 1
        val toBeAdded = BeerItem(nextId, newBeer.name, newBeer.description, newBeer.ABV)
        beerList += toBeAdded
        Created(Json.toJson(toBeAdded))
      case None =>
        BadRequest
    }
  }

  def getAllBeers() = Action {
    if (beerList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(beerList))
    }
  }

  def getBeerById(itemId: Long) = Action {
    val foundItem = beerList.find(_.id == itemId)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def deleteBeerById(itemId: Long) = Action {
    beerList.filterInPlace(_.id != itemId)
    Accepted
  }
}
