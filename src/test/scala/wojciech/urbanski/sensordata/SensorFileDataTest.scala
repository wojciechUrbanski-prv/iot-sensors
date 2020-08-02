package wojciech.urbanski.sensordata

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import wojciech.urbanski.humidity.HumidityIntValue
import wojciech.urbanski.humidity.HumidityNaN

class SensorFileDataTest extends AnyWordSpec with Matchers {

  "Reading sensor data from file" should {

    "result in correctly parsed line if it is in format $sensorId,$humidityValue and humidityValue is integer value between 0 and 100" in {
      SensorFileData("id1,10") shouldBe Some(SensorFileData(SensorId("id1"), HumidityIntValue(10)))
    }

    "result in correctly parsed line if it is in format $sensorId,$humidityValue with extra spaces between" in {
      SensorFileData(" id1  ,   10  ") shouldBe Some(SensorFileData(SensorId("id1"), HumidityIntValue(10)))
    }

    "result in correctly parsed line but humidity is represented as NaN, if line is in format $sensorId,$humidityValue but humidityValue is not a integer" in {
      SensorFileData("id1,c") shouldBe Some(SensorFileData(SensorId("id1"), HumidityNaN))
    }

    "result in correctly parsed line but humidity is represented as NaN, if line is in format $sensorId,$humidityValue but humidityValue is less than 0" in {
      SensorFileData("id1,-1") shouldBe Some(SensorFileData(SensorId("id1"), HumidityNaN))
    }

    "result in correctly parsed line but humidity is represented as NaN, if line is in format $sensorId,$humidityValue but humidityValue is bigger than 100" in {
      SensorFileData("id1,101") shouldBe Some(SensorFileData(SensorId("id1"), HumidityNaN))
    }

    "result in incorrectly parsed line if it is in any different format than $sensorId,$humidityValue" in {
      SensorFileData("id1,101,x") shouldBe None
      SensorFileData("") shouldBe None
      SensorFileData("abc") shouldBe None
      SensorFileData("id1:101") shouldBe None
    }

    "result in not creating SensorFileData if given line is a header one" in {
      SensorFileData("sensor-id,humidity") shouldBe None
    }

  }

}
