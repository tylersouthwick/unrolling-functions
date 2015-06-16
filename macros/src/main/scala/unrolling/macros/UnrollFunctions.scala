package unrolling.macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 *
 */
object UnrollFunctions {

  def skipping(): Unit = {System.out.println("skipping")}

  def leaf(f: () => Unit): Unit = f()

  def unroll(f: () => Unit): Unit = macro unroll_impl

  def unroll_impl(c: whitebox.Context)(f: c.Tree): c.Tree = logIt { log =>
    import c.universe._
    def println(a: Any) = log.println(a)

    object nameFinder extends Traverser {
      var applies = List[Tree]()

      override def traverse(tree: Tree): Unit = tree match {
        case app@Apply(fun, args) =>
          if ("unrolling.macros.UnrollFunctions.leaf".equals(fun.toString())) {
            applies = app :: applies
          }
          super.traverse(fun)
          super.traverseTrees(args)
        case _ => super.traverse(tree)
      }
    }

    nameFinder(f)
    val leaves = nameFinder.applies
    //println("found")
    //leaves.foreach(println)

    val paths = for {
      leaf <- leaves
    } yield {
        val otherLeaves = leaves.filterNot(_.equalsStructure(leaf))
        object leafRemover extends Transformer {
          override def transform(tree: c.universe.Tree): c.universe.Tree = {
            if (otherLeaves.exists(_.equalsStructure(tree))) {
              reify{skipping()}.tree
            } else {
              super.transform(tree)
            }
          }
        }
        val newTree = leafRemover.transform(f)
        println("newTree")
        println(showCode(newTree))
        newTree
      }

    /*
    for {path <- paths} {
      println("found a path")
      println(path)
      println("\n\n\n")
    }
    */
    Block(paths.flatMap(_.children), Literal(Constant()))
    //println("returning" + r)
  }

  import java.io._

  def logIt[A](f: PrintWriter => A): A = {
    val out = new PrintWriter(new OutputStreamWriter(new FileOutputStream("/tmp/macro_dev", true)))
    try {
      f(out)
    } finally {
      out.close()
    }
  }

  case class Node[A](items: Seq[A])

  /*
  case class Node[A](count : Int, value : A) {
    val children = new Array[A](count)

    def add(a : A, level : Int): Unit = {

    }
  }

  class Paths[A]() {
    val head = Node(1)
    var level = 0

    def add(a : A) = {
      head.add(a, level)
    }
    def bifurcate(): Unit = {
      head.bifurcate(level)
    }
    def pop(): Unit = {
      head.pop()
    }
  }
  */
}
