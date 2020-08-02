package wojciech.urbanski.sensordata

import wojciech.urbanski.humidity.{HumidityIntValue, HumidityNaN}

sealed trait SensorData
case class NewValueFromSensor private[sensordata] (sensorId: SensorId, humidityValue: HumidityIntValue) extends SensorData
case class NaNValueFromSensor private[sensordata] (sensorId: SensorId)                                  extends SensorData
case object NotCountableData                                                                            extends SensorData

object SensorData {

  def fromFileData(maybeLineValue: Option[SensorFileData]): SensorData = {
    maybeLineValue.fold(NotCountableData: SensorData) { lineValue =>
      lineValue.humidity match {
        case humidity @ HumidityIntValue(_) => NewValueFromSensor(lineValue.sensorId, humidity)
        case HumidityNaN                    => NaNValueFromSensor(lineValue.sensorId)
      }
    }
  }
}
