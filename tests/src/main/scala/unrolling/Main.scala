package unrolling

/**
 *
 */
object Main {

  import unrolling.macros.UnrollFunctions._

  def main(args : Array[String]) : Unit = {
    unroll { () =>
      println("hello world1")
      leaf { () =>
        println("hello world2")
      }
      branch { () =>
        println("Branch 1 start")
        leaf { () =>
          println("hello world3")
        }
        leaf { () =>
          println("hello world4")
        }
        println("Branch 1 end2")
      }
      println("done")
    }
  }
}
