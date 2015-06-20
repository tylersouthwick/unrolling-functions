package unrolling.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 *
 */
object UnrollFunctions {

  def leaf(f: => Unit): Unit = f
  def branch(f: => Unit): Unit = f

  def unroll(target : ( () => Unit) => Unit)(f: => Unit): Unit = macro unroll_impl

  def unroll_impl(c: blackbox.Context)(target : c.Tree) (f: c.Tree): c.Tree = logIt { log =>
    import c.universe._
    def println(a: Any) = log.println(a)

    val leaves = {
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
      nameFinder.applies.reverse
    }

    val paths = for {
      leaf <- leaves
    } yield {
        val otherLeaves = leaves.filterNot(_.equalsStructure(leaf))
        object leafRemover extends Transformer {
          var found = false
          override def transform(tree: c.universe.Tree): c.universe.Tree = {
            if (tree == leaf) {
              found = true
              leaf
            } else if (otherLeaves.exists(_.equalsStructure(tree))) {
              c.typecheck(reify(null).tree)
            } else {
              tree match {
                case app@Apply(fun, args) =>
                  if (found && "unrolling.macros.UnrollFunctions.branch".equals(fun.toString())) {
                    c.typecheck(reify(null).tree)
                  } else {
                    super.transform(tree)
                  }
                case _ => super.transform(tree)
              }
            }
          }
        }
        leafRemover.transform(f)
      }

    println("target: " + showRaw(target))
    val r = target.collect {
      case Apply(fun, _) => fun
    }
    val applyFun = r.head
    println("applyFun: " + applyFun)
    Block(paths.map(path => Apply(applyFun, List(Function(List(), path)))), Literal(Constant()))
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

}
