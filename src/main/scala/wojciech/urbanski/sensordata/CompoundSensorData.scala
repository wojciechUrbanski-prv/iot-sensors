package wojciech.urbanski.sensordata

import wojciech.urbanski.humidity.{AverageHumidityBigDecimalValue, AverageHumidityValue, AverageNaN, HumidityIntValue, HumidityValue, HumidityNaN}
import wojciech.urbanski.measurements.Measurements

case class CompoundSensorData(minVal: HumidityValue, avgVal: AverageHumidityValue, maxVal: HumidityValue, successfulMeasurements: Measurements) {

  def addNewHumidityValue(humidityValue: HumidityValue): CompoundSensorData =
    humidityValue match {
      case humidity @ HumidityIntValue(_) =>
        CompoundSensorData(
          minVal = findValue(humidity, minVal, Ordering[Int].min),
          avgVal = calculateAverage(humidity, avgVal, successfulMeasurements),
          maxVal = findValue(humidity, maxVal, Ordering[Int].max),
          successfulMeasurements = successfulMeasurements.inc()
        )
      case HumidityNaN => this
    }

  private def findValue(newHumidityValue: HumidityIntValue, currentValue: HumidityValue, findBy: (Int, Int) => Int) =
    currentValue match {
      case HumidityIntValue(value) => HumidityIntValue(findBy(value, newHumidityValue.value))
      case HumidityNaN                      => newHumidityValue
    }

  private def calculateAverage(newHumidityValue: HumidityIntValue, currentAvg: AverageHumidityValue, successfullMeasurements: Measurements) =
    currentAvg match {
      case AverageHumidityBigDecimalValue(value) =>
        AverageHumidityBigDecimalValue((value * successfullMeasurements.value + newHumidityValue.value) / successfullMeasurements.inc().value)
      case AverageNaN =>
        AverageHumidityBigDecimalValue(BigDecimal(newHumidityValue.value))
    }

}

object CompoundSensorData {
  val empty: CompoundSensorData = CompoundSensorData(HumidityNaN, AverageNaN, HumidityNaN, Measurements.zero)
}
