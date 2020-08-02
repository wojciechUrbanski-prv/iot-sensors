package wojciech.urbanski.sensordata

import wojciech.urbanski.humidity.{HumidityIntValue, HumidityNaN, HumidityValue}

final case class SensorFileData protected[sensordata] (sensorId: SensorId, humidity: HumidityValue)

object SensorFileData {

  def apply(line: String): Option[SensorFileData] = {
    line.filterNot(_.isWhitespace).split(',').toList match {
      case "sensor-id" :: "humidity" :: Nil => None
      case sensorId :: "NaN" :: Nil         => Some(SensorFileData(SensorId(sensorId), HumidityNaN))
      case sensorId :: humidityValue :: Nil =>
        humidityValue.toIntOption match {
          case Some(humidityValue) if humidityValue >= 0 && humidityValue <= 100 => Some(SensorFileData(SensorId(sensorId), HumidityIntValue(humidityValue)))
          case Some(_)                                                           => Some(SensorFileData(SensorId(sensorId), HumidityNaN))
          case None                                                              => Some(SensorFileData(SensorId(sensorId), HumidityNaN))
        }
      case _ => None
    }
  }

}
