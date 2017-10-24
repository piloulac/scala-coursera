package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * Created by pierrelouislacorte on 13/09/2017.
  */
@RunWith(classOf[JUnitRunner])
class MainTest extends FunSuite{
  import Main._

  test("Pascal Triangle coordinates (1,3) gives 3 ") {
    assert(pascal(1,3) === 3)
  }

  test("Pascal Triangle coordinates (3,6) gives 20 ") {
    assert(pascal(3,6) === 20)
  }

  test("Parenthesis balancing of '(just an) example' ") {
    assert(balance("(just an) example".toList))
  }

  test("Parenthesis balancing of '())(' ") {
    assert(balance("())(".toList))
  }

  test("Count change for 4CHF with coins of 1,2"){
    assert(countChange(4, List(1,2)) === 3)
  }

  test("Check sort list requirement in count change"){
    assert(countChange(4, List(2,1)) === 3)
  }

  test("Count change for 100CHF with coins of 1,5,10,25,50"){
    assert(countChange(100, List(1,5,10,25,50)) === 292)
  }
}
