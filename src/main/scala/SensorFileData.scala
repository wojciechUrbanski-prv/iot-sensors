final case class SensorFileData private (sensorId: SensorId, humidity: HumidityValue)

object SensorFileData {

  def apply(line: String): Option[SensorFileData] = {
    line.trim.split(',').toList match {
      case sensorId :: "NaN" :: Nil => Some(SensorFileData(SensorId(sensorId), NaN))
      case sensorId :: humidityValue :: Nil => humidityValue.toIntOption match {
        case Some(humidityValue) if humidityValue >= 0 && humidityValue <= 100 => Some(SensorFileData(SensorId(sensorId), HumidityLongValue(humidityValue)))
        case Some(_) => Some(SensorFileData(SensorId(sensorId), NaN))
        case None => Some(SensorFileData(SensorId(sensorId), NaN))
      }
      case _ => None
    }
  }

}
