package funsets

/**
  * Created by pierrelouislacorte on 18/09/2017.
  */
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  trait TestSets {
    val paramS1 = 1
    val paramS2 = 200
    val paramS3 = 17
    val singleton1 = singletonSet(paramS1)
    val singleton2 = singletonSet(paramS2)
    val singleton3 = singletonSet(paramS3)
    val s1Unions2Unions3 = union(singleton1,union(singleton2,singleton3))
    def propEvenNumber(x:Int):Boolean = (x % 2 == 0)
    def propOddNumber(x:Int):Boolean = !propEvenNumber(x)
    def propConvertOddNumberToEvenNumber(x:Int):Int =
      if(x % 2 == 0) x
      else x+1
  }

  test("singletonSet(1) contains s1 and doesn't contain 0") {
    new TestSets {
      assert(contains(singleton1, 1), "Singleton")
      assert(!contains(singleton1, 0), "Singleton 1 should not contains 0")
    }
  }

  test("union of s1 and s2 should contain s1 and s2") {
    new TestSets {
      val s = union(singleton1, singleton2)
      assert(contains(s, paramS1), "Union 1")
      assert(contains(s, paramS2), "Union 2")
      assert(!contains(s, paramS3), "Union 3")
    }
  }

  test("intersect of s2 and s3 should not contain them") {
    new TestSets {
      val s = intersect(singleton2,singleton3)
      assert(!contains(s, paramS2), "Intersection 1")
      assert(!contains(s, paramS3), "Intersection 2")
    }
  }

  test("intersect s1Unions2Unions3 and s3 should contain only s3") {
    new TestSets {
      val s = intersect(s1Unions2Unions3,singleton3)
      assert(!contains(s, paramS1), "Intersection 1")
      assert(!contains(s, paramS2), "Intersection 2")
      assert(contains(s, paramS3), "Intersection 3")
    }
  }

  test("diff of s1Unions2Unions3 and s3 should contain s1 and s2 ") {
    new TestSets {
      val s = diff(s1Unions2Unions3,singleton3)
      assert(contains(s, paramS1), "Diff 1")
      assert(contains(s, paramS2), "Diff 2")
      assert(!contains(s, paramS3), "Diff 3")
    }
  }

  test("Filter s1Unions2Unions3 to get even number") {
    new TestSets {
      val s = filter(s1Unions2Unions3,propEvenNumber)
      assert(!contains(s, paramS1), "Filter 1")
      assert(contains(s, paramS2), "Filter 2")
      assert(!contains(s, paramS3), "Filter 3")
    }
  }

  test("Forall test s1Unions2Unions3 properties?") {
    new TestSets {
      assert(forall(s1Unions2Unions3,(x:Int)=> x>0),"ForAll > 0")
      assert(!forall(s1Unions2Unions3,propEvenNumber),"ForAll Even number")
      assert(!forall(s1Unions2Unions3,propOddNumber),"ForAll odd number?")
    }
  }

  test("Exists test s1Unions2Unions3 properties?") {
    new TestSets {
      assert(exists(s1Unions2Unions3,(x:Int)=> x>100),"Exists > 100")
      assert(exists(s1Unions2Unions3,propEvenNumber),"Exists Even number")
      assert(exists(s1Unions2Unions3,propOddNumber),"Exists odd number?")
      assert(exists(s1Unions2Unions3,propOddNumber)||exists(s1Unions2Unions3,propEvenNumber),"Exists number?")
    }
  }

  test("Map test properties") {
    new TestSets {
      val s = map(s1Unions2Unions3,propConvertOddNumberToEvenNumber)
      assert(!forall(s1Unions2Unions3,propEvenNumber),"ForAll s1Unions2Unions3 not even number")
      assert(forall(s,propEvenNumber),"ForAll s even number")
    }
  }

}
