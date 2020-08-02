package wojciech.urbanski.humidity

import cats.Show
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class HumidityValueTest extends AnyFunSuite with Matchers {

  test("Humidity that contains wrong value should be represented as NaN") {
    Show[HumidityValue].show(NaN) shouldBe "NaN"
  }

  test("Proper humidity value should be represented as a number") {
    Show[HumidityValue].show(HumidityLongValue(15)) shouldBe "15"
  }

}
