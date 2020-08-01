package wojciech.urbanski.sensordata

import wojciech.urbanski.humidity.{HumidityLongValue, NaN}

sealed trait SensorData
case class NewValueFromSensor(sensorId: SensorId, humidityValue: HumidityLongValue) extends SensorData
case class NaNValueFromSensor(sensorId: SensorId) extends SensorData

object SensorData {
  def fromFileData(maybeLineValue: Option[SensorFileData]): Option[SensorData] = {
    maybeLineValue.fold(None: Option[SensorData]) { lineValue => lineValue.humidity match {
      case humidity@HumidityLongValue(_) => Some(NewValueFromSensor(lineValue.sensorId, humidity))
      case NaN => Some(NaNValueFromSensor(lineValue.sensorId))
    }}
  }
}