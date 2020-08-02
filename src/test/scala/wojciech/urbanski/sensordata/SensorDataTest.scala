package wojciech.urbanski.sensordata

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import wojciech.urbanski.humidity.HumidityIntValue

class SensorDataTest extends AnyFunSuite with Matchers {

  test("Sensor data should be treated as new one if data coming from file contains humidity value") {
    SensorData.fromFileData(SensorFileData("id1,10")) shouldBe NewValueFromSensor(SensorId("id1"), HumidityIntValue(10))
  }

  test("Sensor data should be treated as a NaN value if data coming from file contains wrong humidity value") {
    SensorData.fromFileData(SensorFileData("id1,NaN")) shouldBe NaNValueFromSensor(SensorId("id1"))
  }

  test("Sensor data should be treated as NotCountable if there was no data retrieved from file") {
    SensorData.fromFileData(None) shouldBe NotCountableData
  }

}
