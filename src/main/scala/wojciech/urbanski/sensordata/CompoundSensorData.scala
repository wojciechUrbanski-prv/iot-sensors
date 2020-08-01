package wojciech.urbanski.sensordata

import wojciech.urbanski.humidity.{AverageHumidityBigDecimalValue, AverageHumidityValue, AverageNaN, HumidityLongValue, HumidityValue, NaN}
import wojciech.urbanski.measurements.Measurements

case class CompoundSensorData(minVal: HumidityValue, avgVal: AverageHumidityValue, maxVal: HumidityValue, successfulMeasurements: Measurements) {

  def addNewHumidityValue(humidityValue: HumidityValue): CompoundSensorData = humidityValue match {
    case humidity@HumidityLongValue(_) =>
      CompoundSensorData(
        minVal = findValue(humidity, minVal, Ordering[Long].min),
        avgVal = calculateAverage(humidity, avgVal, successfulMeasurements),
        maxVal = findValue(humidity, maxVal, Ordering[Long].max),
        successfulMeasurements = successfulMeasurements.inc()
      )
    case NaN => this
  }

  private def findValue(newHumidityValue: HumidityLongValue, currentValue: HumidityValue, findBy: (Long, Long) => Long) = currentValue match {
    case HumidityLongValue(value) => HumidityLongValue(findBy(value, newHumidityValue.value))
    case NaN => newHumidityValue
  }

  private def calculateAverage(newHumidityValue: HumidityLongValue, currentAvg: AverageHumidityValue, successfullMeasurements: Measurements) = currentAvg match {
    case AverageHumidityBigDecimalValue(value) =>
      AverageHumidityBigDecimalValue((value * successfullMeasurements.value + newHumidityValue.value) / successfullMeasurements.inc().value)
    case AverageNaN =>
      AverageHumidityBigDecimalValue(BigDecimal(newHumidityValue.value))
  }

}

object CompoundSensorData {
  val empty: CompoundSensorData = CompoundSensorData(NaN, AverageNaN, NaN, Measurements.zero)
}