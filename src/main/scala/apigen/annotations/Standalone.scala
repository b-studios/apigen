package apigen

package standalone

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

class accessors extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro AccessorMacros.accessors
}

object AccessorMacros {

  def accessors(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    annottees.map(_.tree).toList match {
      case ValDef(_, _, tpt, _) :: (enclosing: ClassDef) :: _ => { 
        
        val tpe = c.typeCheck(q"(null.asInstanceOf[$tpt])").tpe

        println(tpe.members)

        c.Expr(Block(List(enclosing), Literal(Constant(()))))
      }
    }
  }
}