package wojciech.urbanski.statistics

import java.nio.file.Path

import cats.Show
import wojciech.urbanski.humidity.{AverageHumidityValue, HumidityNaN, HumidityValue}
import wojciech.urbanski.measurements.Measurements
import wojciech.urbanski.sensordata.{CompoundSensorData, NaNValueFromSensor, NewValueFromSensor, NotCountableData, SensorData, SensorId}

case class OverallStatistics(
  processedFiles: Set[String],
  processedMeasurements: Measurements,
  failedMeasurements: Measurements,
  sensorsData: Map[SensorId, CompoundSensorData]
) {

  def addNewSensorData(file: Path, sensorData: SensorData): OverallStatistics =
    sensorData match {
      case NewValueFromSensor(sensorId, humidityValue) =>
        OverallStatistics(processedFiles + file.toString, processedMeasurements.inc(), failedMeasurements, addNewValueToCompoundStatistics(sensorId, humidityValue))
      case NaNValueFromSensor(sensorId) =>
        OverallStatistics(processedFiles + file.toString, processedMeasurements.inc(), failedMeasurements.inc(), addNewValueToCompoundStatistics(sensorId, HumidityNaN))
      case NotCountableData => updateFiles(file)
    }

  private def addNewValueToCompoundStatistics(sensorId: SensorId, newValue: HumidityValue) =
    sensorsData.updatedWith(sensorId) {
      case Some(compoundSensorData) =>
        Some(compoundSensorData.addNewHumidityValue(newValue))
      case None =>
        Some(CompoundSensorData.empty.addNewHumidityValue(newValue))
    }

  private def updateFiles(file: Path): OverallStatistics = this.copy(processedFiles = processedFiles + file.toString)

}

object OverallStatistics {
  val empty: OverallStatistics = OverallStatistics(Set.empty, Measurements.zero, Measurements.zero, Map.empty)

  implicit val overallStatisticsShow: Show[OverallStatistics] = (overallStatistics: OverallStatistics) => {
    val firstHeader  = s"Num of processed files: ${overallStatistics.processedFiles.size}\n"
    val secondHeader = s"Num of processed measurements: ${overallStatistics.processedMeasurements.value}\n"
    val thirdHeader  = s"Num of failed measurements: ${overallStatistics.failedMeasurements.value}\n"
    val midTitle     = "\nSensors with highest avg humidity:\n"

    overallStatistics.sensorsData.toList
      .sortBy {
        case (_, sensorData) => sensorData.avgVal
      }(Ordering[AverageHumidityValue].reverse)
      .map {
        case (sensorId, sensorData) =>
          s"${sensorId.id} , ${Show[HumidityValue].show(sensorData.minVal)} , ${Show[AverageHumidityValue].show(sensorData.avgVal)} , ${Show[HumidityValue].show(sensorData.maxVal)}"
      }
      .mkString(firstHeader + secondHeader + thirdHeader + midTitle, "\n", "")
  }

}
