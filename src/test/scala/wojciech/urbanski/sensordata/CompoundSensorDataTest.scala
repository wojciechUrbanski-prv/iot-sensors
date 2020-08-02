package wojciech.urbanski.sensordata

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import wojciech.urbanski.humidity.{AverageHumidityBigDecimalValue, AverageNaN, HumidityIntValue, HumidityNaN}
import wojciech.urbanski.measurements.Measurements

class CompoundSensorDataTest extends AnyFunSuite with Matchers {

  val compoundSensorDataWithSomeInitialState: CompoundSensorData = CompoundSensorData(
    HumidityIntValue(5),
    AverageHumidityBigDecimalValue(BigDecimal(10)),
    HumidityIntValue(20),
    Measurements(3)
  )

  test("Empty CompoundSensorData should have set up NaNs in each field and measurements should be set up to 0") {
    CompoundSensorData.empty shouldBe CompoundSensorData(HumidityNaN, AverageNaN, HumidityNaN, Measurements.zero)
  }

  test("Adding new sensorData to empty CompoundSensorData should increase measurements by one and set up all humidity fields to the provided one") {
    CompoundSensorData.empty.addNewHumidityValue(HumidityIntValue(10)) shouldBe CompoundSensorData(
      HumidityIntValue(10),
      AverageHumidityBigDecimalValue(BigDecimal(10)),
      HumidityIntValue(10),
      Measurements.one
    )
  }

  test("Adding new minimum sensorData to CompoundSensorData should result in replacement of minValue and recalculation of average value") {
    compoundSensorDataWithSomeInitialState.addNewHumidityValue(HumidityIntValue(4)) shouldBe CompoundSensorData(
      HumidityIntValue(4),
      AverageHumidityBigDecimalValue(BigDecimal(8.5)),
      HumidityIntValue(20),
      compoundSensorDataWithSomeInitialState.successfulMeasurements.inc()
    )
  }

  test("Adding new maximum sensorData to CompoundSensorData should result in replacement of minValue and recalculation of average value") {
    compoundSensorDataWithSomeInitialState.addNewHumidityValue(HumidityIntValue(34)) shouldBe CompoundSensorData(
      HumidityIntValue(5),
      AverageHumidityBigDecimalValue(BigDecimal(16)),
      HumidityIntValue(34),
      compoundSensorDataWithSomeInitialState.successfulMeasurements.inc()
    )
  }

  test(
    "Adding new sensorData that is between minimum and maximum values to CompoundSensorData should result in replacement of minValue and recalculation of average value"
  ) {
    compoundSensorDataWithSomeInitialState.addNewHumidityValue(HumidityIntValue(15)) shouldBe CompoundSensorData(
      HumidityIntValue(5),
      AverageHumidityBigDecimalValue(BigDecimal(11.25)),
      HumidityIntValue(20),
      compoundSensorDataWithSomeInitialState.successfulMeasurements.inc()
    )
  }

  test("Adding NaN value to CompoundSensorData should not do anything") {
    compoundSensorDataWithSomeInitialState.addNewHumidityValue(HumidityNaN) shouldBe compoundSensorDataWithSomeInitialState
  }

}
