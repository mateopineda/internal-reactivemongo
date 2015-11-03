package models


import org.joda.time.DateTime

import play.api.data._
import play.api.data.Forms.{ text, longNumber, mapping, nonEmptyText, optional }
import play.api.data.validation.Constraints.pattern

import reactivemongo.bson.{
  BSONDateTime, BSONDocument, BSONObjectID
}

case class Movement(
  id: Option[String],
  valueMovement: String,
  description: String,
  dateMovement: Option[DateTime])

// Turn off your mind, relax, and float downstream
// It is not dying...
object Movement {
  import play.api.libs.json._

  implicit object MovementWrites extends OWrites[Movement] {
    def writes(movement: Movement): JsObject = Json.obj(
      "_id" -> movement.id,
      "valueMovement" -> movement.valueMovement,
      "description" -> movement.description,
      "dateMovement" -> movement.dateMovement.fold(-1L)(_.getMillis))
  }

  implicit object MovementReads extends Reads[Movement] {
    def reads(json: JsValue): JsResult[Movement] = json match {
      case obj: JsObject => try {
        val id = (obj \ "_id").asOpt[String]
        val valueMovement = (obj \ "valueMovement").as[String]
        val description = (obj \ "description").as[String]
        val dateMovement = (obj \ "dateMovement").asOpt[Long]

        JsSuccess(Movement(id, valueMovement, description,
          dateMovement.map(new DateTime(_))))
        
      } catch {
        case cause: Throwable => JsError(cause.getMessage)
      }

      case _ => JsError("expected.jsobject")
    }
  }

  val form = Form(
    mapping(
      "id" -> optional(text verifying pattern(
        """[a-fA-F0-9]{24}""".r, error = "error.objectId")),
      "valueMovement" -> nonEmptyText,
      "description" -> text,
      "dateMovement" -> optional(longNumber)) {
      (id, valueMovement, description, dateMovement) =>
      Movement(
        id,
        valueMovement,
        description,
        dateMovement.map(new DateTime(_)))
    } { movement =>
      Some(
        (movement.id,
          movement.valueMovement,
          movement.description,
          movement.dateMovement.map(_.getMillis)))
    })
}
