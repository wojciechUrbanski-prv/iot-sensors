package wojciech.urbanski.statistics

import java.nio.file.Paths

import cats.effect.{ContextShift, IO}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import wojciech.urbanski.humidity.{AverageHumidityBigDecimalValue, AverageNaN, HumidityIntValue, HumidityNaN}
import wojciech.urbanski.measurements.Measurements
import wojciech.urbanski.sensordata.{CompoundSensorData, SensorId}

import scala.concurrent.ExecutionContext

class ProcessFilesTest extends AnyFunSuite with Matchers {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val id1AverageHumidity: BigDecimal = (BigDecimal(10) + BigDecimal(15) + BigDecimal(10)) / 3
  val id2AverageHumidity: BigDecimal = (BigDecimal(25) + BigDecimal(15) + BigDecimal(25)+ BigDecimal(15)+ BigDecimal(30)+ BigDecimal(17)) / 6
  val id3AverageHumidity: BigDecimal = (BigDecimal(45) + BigDecimal(1) + BigDecimal(0) + BigDecimal(45)) / 4

  test("ProcessFiles should successfully read all .csv files in given directory and construct from them correct overall statistics") {
    val statistics = new ProcessFiles[IO]().readFromFilesAndGatherStatistics(Paths.get("csvtestdata/")).unsafeRunSync()
    statistics shouldBe OverallStatistics(
      Set("csvtestdata/csv1.csv", "csvtestdata/csv2.csv", "csvtestdata/csv3.csv"),
      Measurements(19),
      Measurements(5),
      Map(
        SensorId("id1") -> CompoundSensorData(HumidityIntValue(10), AverageHumidityBigDecimalValue(id1AverageHumidity), HumidityIntValue(15), Measurements(3)),
        SensorId("id2") -> CompoundSensorData(HumidityIntValue(15), AverageHumidityBigDecimalValue(id2AverageHumidity), HumidityIntValue(30), Measurements(6)),
        SensorId("id3") -> CompoundSensorData(HumidityIntValue(0), AverageHumidityBigDecimalValue(id3AverageHumidity), HumidityIntValue(45), Measurements(4)),
        SensorId("id4") -> CompoundSensorData(HumidityIntValue(90), AverageHumidityBigDecimalValue(BigDecimal(90)), HumidityIntValue(90), Measurements.one),
        SensorId("id5") -> CompoundSensorData(HumidityNaN, AverageNaN, HumidityNaN, Measurements.zero)
      )
    )
  }

  test("ProcessFiles should return empty statistics if given directory is empty") {
    val statistics = new ProcessFiles[IO]().readFromFilesAndGatherStatistics(Paths.get("emptydirectory/")).unsafeRunSync()
    statistics shouldBe OverallStatistics.empty
  }

  test("ProcessFiles should return empty statistics if given directory does not contains .csv files") {
    val statistics = new ProcessFiles[IO]().readFromFilesAndGatherStatistics(Paths.get("nocsvfiles/")).unsafeRunSync()
    statistics shouldBe OverallStatistics.empty
  }

}
