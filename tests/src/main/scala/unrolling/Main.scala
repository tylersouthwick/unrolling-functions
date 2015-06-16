package unrolling

/**
 *
 */
object Main {

  import unrolling.macros.UnrollFunctions._

  def main(args : Array[String]) : Unit = {
    unroll { () =>
      println("hello world")
      leaf { () =>
        println("hello world2")
      }
      leaf { () =>
        println("hello world3")
      }
      println("dne")
    }
  }
}
