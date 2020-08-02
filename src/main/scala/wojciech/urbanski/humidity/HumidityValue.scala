package wojciech.urbanski.humidity

import cats.Show

sealed trait HumidityValue
case class HumidityIntValue(value: Int) extends HumidityValue
case object HumidityNaN                           extends HumidityValue

object HumidityValue {

  implicit val showInstance: Show[HumidityValue] = {
    case HumidityIntValue(value) => value.toString
    case HumidityNaN                      => "NaN"
  }
}
