package wojciech.urbanski.humidity

import cats.Show

import scala.math.BigDecimal.RoundingMode

sealed trait AverageHumidityValue
case class AverageHumidityBigDecimalValue(value: BigDecimal) extends AverageHumidityValue
case object AverageNaN                                       extends AverageHumidityValue

object AverageHumidityValue {

  implicit val ordering: Ordering[AverageHumidityValue] = (x: AverageHumidityValue, y: AverageHumidityValue) =>
    (x, y) match {
      case (AverageNaN, AverageHumidityBigDecimalValue(_))                                  => -1
      case (AverageHumidityBigDecimalValue(_), AverageNaN)                                  => 1
      case (AverageNaN, AverageNaN)                                                         => 0
      case (AverageHumidityBigDecimalValue(xValue), AverageHumidityBigDecimalValue(yValue)) => Ordering[BigDecimal].compare(xValue, yValue)
    }

  implicit val showInstance: Show[AverageHumidityValue] = {
    case AverageHumidityBigDecimalValue(value) => value.setScale(0, RoundingMode.HALF_UP).toString()
    case AverageNaN                            => "NaN"
  }

}
