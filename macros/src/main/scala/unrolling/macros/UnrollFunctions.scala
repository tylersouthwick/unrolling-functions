package unrolling.macros

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 *
 */
case object Leaf {
  def apply(note : String)(f : => Unit) : Unit = f
}
case object Branch {
  def apply(note: String)(f: => Unit): Unit = f
}
object UnrollFunctions {

  def unroll(target : (Seq[String], () => Unit) => Unit)(f: => Unit): Unit = macro unroll_impl

  def unroll_impl(c: blackbox.Context)(target : c.Tree) (f: c.Tree): c.Tree = {
    import c.universe._

    implicit class LeafBranchFinder(tree : Tree) {
      def isBranch = tree.toString().startsWith("unrolling.macros.UnrollFunctions.branch(")
      def isLeaf = {
        println("possible leaf: " + tree.toString() + " -> " + tree.tpe)
        println(showRaw(tree))
        tree.toString().startsWith("unrolling.macros.UnrollFunctions.leaf(")
      }
    }

    val leaves = {
      object nameFinder extends Traverser {
        var applies = List[Apply]()

        override def traverse(tree: Tree): Unit = tree match {
          case app@Apply(fun, args) =>
            if (fun.isLeaf) {
              c.info(fun.pos, "Found leaf", force = false)
              applies = app :: applies
            } /*else {
              println("found fun: " + fun.toString() + " [" + showRaw(fun) + "]")
            }*/
            super.traverse(fun)
            super.traverseTrees(args)
          case Select(Select(This(TypeName("MacroSpec")), TermName("leaf"))) =>
            println("found leaf?")
          case _ => {
            //println("found: " + tree.toString() + " [" + showRaw(tree) + "]")
            super.traverse(tree)
          }
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
          override def transform(tree: c.universe.Tree): c.universe.Tree = {
            if (otherLeaves.exists(_.equalsStructure(tree))) {
              c.info(tree.pos, "Removing leaf from tree " + leaf.fun, force = false)
              c.typecheck(reify(null).tree)
            } else {
              super.transform(tree)
            }
          }
        }
        /**
         * branches need to be removed in a second pass to handle a case such as
         *
         * branch {
         * }
         * branch {
         *   leaf {
         *   }
         * }
         *
         * Where the first branch needs to be removed
         */
        object branchRemover extends Transformer {
          override def transform(tree: c.universe.Tree): c.universe.Tree = {
            if (tree.isBranch) {
              tree match {
                case app@Apply(Apply(_, _), _) if !app.exists(_.isLeaf)=> {
                  c.info(app.pos, "Removing branch as it does not apply to leaf: " + leaf.asInstanceOf[Apply].fun, force = false)
                  c.typecheck(reify(null).tree)
                }
                case _ => super.transform(tree)
              }
            } else {
              super.transform(tree)
            }
          }
        }
        val pathWithBranches = leafRemover.transform(f)
        val path = branchRemover.transform(pathWithBranches)
        object PathArgBuilder extends Traverser {
          var args = List[String]()

          override def traverse(tree: c.universe.Tree): Unit = tree match {
            case app@Apply(fun@Apply(_, funArgs), _) if fun.isBranch || fun.isLeaf => {
              //println("looking on " + fun + " - " + showRaw(fun))
              //println("args(" + funArgs.length + ") : " + funArgs)
              val values = funArgs.flatMap(_.collect({
                case Literal(Constant(value)) => Option(value).map(_.toString)
              })).flatten
              args = args ++ values
              //println("values: " + values)
              super.traverse(app)
            }
            case _ => super.traverse(tree)
          }
        }
        PathArgBuilder(path)
        val args = PathArgBuilder.args
        //println("args: " + args)
        path -> args
      }

    //println("target: " + showRaw(target))
    val r = target.collect {
      case Apply(fun, _) => fun
    }
    val applyFun = r.head
    //println("applyFun: " + applyFun)
    Block(paths.map(path => Apply(applyFun, List(q"${path._2}", Function(List(), path._1)))), Literal(Constant()))
  }

}
