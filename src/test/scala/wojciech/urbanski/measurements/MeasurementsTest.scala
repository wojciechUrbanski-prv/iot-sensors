package wojciech.urbanski.measurements

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class MeasurementsTest extends AnyFunSuite with Matchers {

  test("Increasing measurement should result in measurement that has value bigger by one") {
    Measurements(10).inc() shouldBe Measurements(11)
  }

}
