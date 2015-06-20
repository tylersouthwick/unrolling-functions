package unrolling.tests

import org.scalatest.{ShouldMatchers, FunSpec}

import scala.collection.mutable.ListBuffer

class MacroSpec extends FunSpec with ShouldMatchers {

  describe("unroll") {

    var counter = 0

    val buffer = ListBuffer[() => Unit]()
    def testRegistration( f : () => Unit): Unit = {
      buffer += f
    }

    import unrolling.macros.UnrollFunctions._
    unroll(testRegistration) {
      branch {
        counter += 2
        leaf {
          counter += 1
        }
        branch {
          counter += 3
          leaf {
            counter += 2
          }
          leaf {
            counter += 7
          }
        }
      }
    }

    val expected = Seq(
      3, 7, 12
    )
    for {
      t <- expected.zipWithIndex
      value = t._1
      index = t._2
    } {
      it(s"should resolve #$index to $value") {
        val f = buffer(index)
        counter = 0
        f.apply()
        counter should equal(value)
      }
    }
  }
}
