package wojciech.urbanski.statistics

import java.nio.file.Paths

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import wojciech.urbanski.humidity.{AverageHumidityBigDecimalValue, HumidityIntValue}
import wojciech.urbanski.measurements.Measurements
import wojciech.urbanski.sensordata.{CompoundSensorData, NotCountableData, SensorData, SensorFileData, SensorId}

class OverallStatisticsTest extends AnyFunSuite with Matchers {

  val sensor1Humidity10: SensorData  = SensorData.fromFileData(SensorFileData("id1,10"))
  val sensor2Humidity35: SensorData  = SensorData.fromFileData(SensorFileData("id2,35"))
  val sensor1Humidity50: SensorData  = SensorData.fromFileData(SensorFileData("id1,50"))
  val sensor1HumidityNaN: SensorData = SensorData.fromFileData(SensorFileData("id1,NaN"))
  val sensor3Humidity0: SensorData   = SensorData.fromFileData(SensorFileData("id3,0"))

  test("Adding new data to empty overall statistics should increase processedMeasurements field, add file to file list and create statistics for given sensor") {
    OverallStatistics.empty.addNewSensorData(Paths.get("file1"), sensor1Humidity10) shouldBe OverallStatistics(
      processedFiles = Set("file1"),
      processedMeasurements = Measurements.one,
      failedMeasurements = Measurements.zero,
      sensorsData =
        Map(SensorId("id1") -> CompoundSensorData(HumidityIntValue(10), AverageHumidityBigDecimalValue(BigDecimal(10)), HumidityIntValue(10), Measurements.one))
    )
  }

  test(
    "Adding new data to overall statistics should increase processedMeasurements fields, do not add file to file list because it is already there and update statistics for given sensor"
  ) {
    OverallStatistics.empty.addNewSensorData(Paths.get("file1"), sensor1Humidity10).addNewSensorData(Paths.get("file1"), sensor1Humidity50) shouldBe OverallStatistics(
      processedFiles = Set("file1"),
      processedMeasurements = Measurements(2),
      failedMeasurements = Measurements.zero,
      sensorsData =
        Map(SensorId("id1") -> CompoundSensorData(HumidityIntValue(10), AverageHumidityBigDecimalValue(BigDecimal(30)), HumidityIntValue(50), Measurements(2)))
    )
  }

  test(
    "Adding NaN data to overall statistics should increase failedMeasurements and processedMeasurements and do not change anything besides"
  ) {
    OverallStatistics.empty
      .addNewSensorData(Paths.get("file1"), sensor1Humidity10)
      .addNewSensorData(Paths.get("file1"), sensor1Humidity50)
      .addNewSensorData(Paths.get("file1"), sensor1HumidityNaN) shouldBe OverallStatistics(
      processedFiles = Set("file1"),
      processedMeasurements = Measurements(3),
      failedMeasurements = Measurements.one,
      sensorsData =
        Map(SensorId("id1") -> CompoundSensorData(HumidityIntValue(10), AverageHumidityBigDecimalValue(BigDecimal(30)), HumidityIntValue(50), Measurements(2)))
    )
  }

  test(
    "Adding NotCountable data to overall statistics should change only processedFiles parameter"
  ) {
    OverallStatistics.empty
      .addNewSensorData(Paths.get("file1"), sensor1Humidity10)
      .addNewSensorData(Paths.get("file1"), sensor1Humidity50)
      .addNewSensorData(Paths.get("file1"), sensor1HumidityNaN)
      .addNewSensorData(Paths.get("file2"), NotCountableData) shouldBe OverallStatistics(
      processedFiles = Set("file1", "file2"),
      processedMeasurements = Measurements(3),
      failedMeasurements = Measurements.one,
      sensorsData =
        Map(SensorId("id1") -> CompoundSensorData(HumidityIntValue(10), AverageHumidityBigDecimalValue(BigDecimal(30)), HumidityIntValue(50), Measurements(2)))
    )
  }

  test(
    "Adding NaN data from another file to overall statistics should increase failedMeasurements and processedMeasurements and update field list"
  ) {
    OverallStatistics.empty
      .addNewSensorData(Paths.get("file1"), sensor1Humidity10)
      .addNewSensorData(Paths.get("file1"), sensor1Humidity50)
      .addNewSensorData(Paths.get("file2"), sensor1HumidityNaN) shouldBe OverallStatistics(
      processedFiles = Set("file1", "file2"),
      processedMeasurements = Measurements(3),
      failedMeasurements = Measurements.one,
      sensorsData =
        Map(SensorId("id1") -> CompoundSensorData(HumidityIntValue(10), AverageHumidityBigDecimalValue(BigDecimal(30)), HumidityIntValue(50), Measurements(2)))
    )
  }

  test("Adding several new data from different files should update all measurements fields and update sensor statistics") {
    OverallStatistics.empty
      .addNewSensorData(Paths.get("file1"), sensor1Humidity10)
      .addNewSensorData(Paths.get("file3"), sensor2Humidity35)
      .addNewSensorData(Paths.get("file1"), sensor1Humidity50)
      .addNewSensorData(Paths.get("file2"), sensor1HumidityNaN)
      .addNewSensorData(Paths.get("file1"), sensor3Humidity0) shouldBe OverallStatistics(
      processedFiles = Set("file1", "file2", "file3"),
      processedMeasurements = Measurements(5),
      failedMeasurements = Measurements.one,
      sensorsData = Map(
        SensorId("id1") -> CompoundSensorData(HumidityIntValue(10), AverageHumidityBigDecimalValue(BigDecimal(30)), HumidityIntValue(50), Measurements(2)),
        SensorId("id2") -> CompoundSensorData(HumidityIntValue(35), AverageHumidityBigDecimalValue(BigDecimal(35)), HumidityIntValue(35), Measurements.one),
        SensorId("id3") -> CompoundSensorData(HumidityIntValue(0), AverageHumidityBigDecimalValue(BigDecimal(0)), HumidityIntValue(0), Measurements.one)
      )
    )
  }
}
