package apigen
package annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import java.util.regex.Pattern
import scala.reflect.macros.{ Context, TypecheckException }
import scala.reflect.internal

trait Utils[C <: Context] {

  val c: C

  import c.universe._
  
  /**
   * Converts a given type tree `t` into the type it represents.
   *
   * Can abort the macro execution if the tree cannot be typechecked. Most of the times this is
   * due to a misplaced import statement.
   *
   * Does not work for aliases due to this bug:
   *   https://github.com/aztek/scala-workflow/issues/2#issuecomment-26532778
   */
  private[annotations] def toType(tpt: c.Tree): c.Type = {

    if (tpt.tpe != null)
      return tpt.tpe

    try {
      c.typeCheck(q"(null.asInstanceOf[$tpt])").tpe
    } catch {
      case e:scala.reflect.macros.TypecheckException => 
        val msg = s"""Could not find type for: $tpt
                     |Make sure the given type is in scope.
                     |Import statements inside of a class definition might not work.
                     |Try moving them out of the class-definition and prevent to use aliases.
                     |Original exception: 
                     |$e"""
        c.abort(c.enclosingPosition, msg.stripMargin)
    }
  }
  private[annotations] def assembleResult(ts: Tree*): c.Expr[Any] = c.Expr(Block(ts.toList, Literal(Constant(()))))

  private[annotations] implicit class MethodSymbolWrapper(self: MethodSymbol) {

    def isAbstract: Boolean = self
      .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
      .hasFlag(scala.reflect.internal.Flags.DEFERRED)

    def isDeprecated: Boolean = self
      .asInstanceOf[scala.reflect.internal.Symbols#Symbol]
      .isDeprecated

    /**
     * Checks whether `method` has already been defined in some super class of
     * `cls`.
     *
     * @example {{{
     *   trait A { def foo: Int }
     *   trait B extends A { def foo = 42; def bar = "hello" }
     *   ...
     *   scala> isFirstOccurrence(<method foo>, <class B>)
     *   res0: Boolean = false
     *   scala> isFirstOccurrence(<method bar>, <class B>)
     *   res1: Boolean = true
     * }}}
     */
    def isFirstOccurrenceIn(cls: Type): Boolean = self
      .owner.asInstanceOf[internal.Symbols#Symbol]
      .baseClasses.head == cls.baseClasses.head

    def isOverridingSymbol: Boolean = self
      .asInstanceOf[internal.Symbols#Symbol]
      .isOverridingSymbol

    def hasAccessorName: Boolean = 
      "^(is|get|set)".r.findPrefixOf(self.name.toString).isDefined

    // is this the best way to determine varargs? highly fragile through string check
    // baseTypeSeq.toList(1) == Array[T]
    // baseTypeSeq.toList(1).typeArguments.head == T
    // before it does not have type params
    //
    // Probably best to first add new annotations varArgSetter and nonNullableVarArgSetter
    // then elaborate on arguments to annotations to minimize footprint of these names
    def hasVarArgs: Boolean = self.paramss match {
      case List(List(arg)) => {
          val sig = arg.typeSignature.toString
          sig.startsWith("<repeated...>") || sig.endsWith("*")
      }
      case _ => false
    }

  }

  private[annotations] implicit class TypeWrapper(self: Type) {
    def typeArguments: List[Type] = 
      self.asInstanceOf[internal.Types#Type].typeArguments.map(_.asInstanceOf[Type])
  }

  /**
   * returns a list of symbols representing the provided macro arguments
   */
  private[annotations] implicit class ContextWrapper(self: Context) {
    def arguments: List[scala.Symbol] = c.prefix.tree match {
      case q"new $name(..$args)" => args.map( tree => Symbol(tree.toString))
    }
  }

  def let[T, S](x: T)(f: T => S): S = f(x)

}

/**
 * A utility which ensures that a code fragment does not typecheck.
 * 
 * Credit: Stefan Zeiger (@StefanZeiger) via shapeless
 */
object illTyped {
  def apply(code: String): Unit = macro applyImplNoExp
  def apply(code: String, expected: String): Unit = macro applyImpl
  
  def applyImplNoExp(c: Context)(code: c.Expr[String]) = applyImpl(c)(code, null)
  
  def applyImpl(c: Context)(code: c.Expr[String], expected: c.Expr[String]): c.Expr[Unit] = {
    import c.universe._

    val Expr(Literal(Constant(codeStr: String))) = code
    val (expPat, expMsg) = expected match {
      case null => (null, "Expected some error.")
      case Expr(Literal(Constant(s: String))) =>
        (Pattern.compile(s, Pattern.CASE_INSENSITIVE), "Expected error matching: "+s)
    }

    try {
      c.typeCheck(c.parse("{ "+codeStr+" }"))
      c.abort(c.enclosingPosition, "Type-checking succeeded unexpectedly.\n"+expMsg)
    } catch {
      case e: TypecheckException =>
        val msg = e.getMessage
        if((expected ne null) && !(expPat.matcher(msg)).matches)
          c.abort(c.enclosingPosition, "Type-checking failed in an unexpected way.\n"+expMsg+"\nActual error: "+msg)
    }
    
    reify(())
  }
}
