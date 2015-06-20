package unrolling.tests

import org.scalatest.{ShouldMatchers, FunSpec}

import scala.collection.mutable.ListBuffer

class MacroSpec extends FunSpec with ShouldMatchers {

  describe("unroll") {

    var counter = 0

    case class RegisteredTest(args : Seq[String], f : () => Unit)
    val buffer = ListBuffer[RegisteredTest]()
    def testRegistration(args : Seq[String], f : () => Unit): Unit = {
      println("found args: " + args)
      buffer += RegisteredTest(args, f)
    }

    import unrolling.macros.UnrollFunctions._
    unroll(testRegistration) {
      leaf("test1") {
        counter += 27
      }
      branch("branch1") {
        counter += 2
        leaf("test1") {
          counter += 1
        }
        branch("branch2") {
          counter += 3
          leaf("test2") {
            counter += 2
          }
          leaf("test3") {
            counter += 7
          }
        }
      }
      branch("branch3") {
        println("branch3")
        counter += 2
        leaf("leaf6") {
          counter += 33
        }
      }
    }

    val expected = Seq(
      27 -> Seq("test1"),
      3 -> Seq("branch1", "test1"),
      7 -> Seq("branch1", "branch2", "test2"),
      12 -> Seq("branch1", "branch2", "test3"),
      35 -> Seq("branch3", "leaf6")
    )
    for {
      t <- expected.zipWithIndex
      value = t._1._1
      path = t._1._2
      index = t._2
    } {
      it(s"should resolve #$index to $value") {
        val registeredTest = buffer(index)
        counter = 0
        registeredTest.f.apply()
        registeredTest.args should equal(path)
        counter should equal(value)
      }
    }
  }
}
