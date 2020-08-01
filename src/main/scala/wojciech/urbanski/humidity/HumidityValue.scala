package wojciech.urbanski.humidity

import cats.Show

sealed trait HumidityValue
case class HumidityLongValue(value: Long) extends HumidityValue
case object NaN extends HumidityValue

object HumidityValue {
  implicit val showInstance: Show[HumidityValue] = {
    case HumidityLongValue(value) => value.toString
    case NaN => "NaN"
  }
}
