package unrolling

/**
 *
 */
object Main {

  import unrolling.macros.UnrollFunctions._

  def register(f : () => Unit): Unit = {
    println("registering: " + f.toString)
    println("invoke")
    f.apply()
    println("end invoke")
  }

  def main(args : Array[String]) : Unit = {
    unroll(register) {
      println("hello world1")
      leaf {
        println("hello world2")
      }
      branch {
        println("Branch 1 start")
        leaf {
          println("hello world3")
        }
        branch {
          println("Branch 2 stat")
          leaf {
            println("hello world4")
          }
          println("Branch 2 end")
        }
        println("Branch 1 end2")
      }
      println("done")
    }
  }
}
