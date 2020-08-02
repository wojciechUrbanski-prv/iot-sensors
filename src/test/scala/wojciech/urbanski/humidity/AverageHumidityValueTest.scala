package wojciech.urbanski.humidity

import cats.Show
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class AverageHumidityValueTest extends AnyFunSuite with Matchers {

  test("Proper average humidity value should have always precedence over NaN value") {
    Ordering[AverageHumidityValue].gt(AverageHumidityBigDecimalValue(BigDecimal(1)), AverageNaN) shouldBe true
    Ordering[AverageHumidityValue].gt(AverageNaN, AverageHumidityBigDecimalValue(BigDecimal(1))) shouldBe false
  }

  test("Two average humidity values represented as NaNs should be equal") {
    Ordering[AverageHumidityValue].equiv(AverageNaN, AverageNaN) shouldBe true
  }

  test("Two proper average humidity values should be ordered as normal numbers") {
    Ordering[AverageHumidityValue].gt(AverageHumidityBigDecimalValue(BigDecimal(3.5)), AverageHumidityBigDecimalValue(BigDecimal(2))) shouldBe true
  }

  test("Average humidity value that can't be represented as number should be represented as NaN") {
    Show[AverageHumidityValue].show(AverageNaN) shouldBe "NaN"
  }

  test("Average humidity value should be represented as Integer with Rounding Half up strategy") {
    Show[AverageHumidityValue].show(AverageHumidityBigDecimalValue(BigDecimal(4.5))) shouldBe "5"
  }

}
