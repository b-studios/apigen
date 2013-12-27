package apigen
package annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

import scala.collection.SeqLike

/**
 * Class Annotations
 */
class accessors extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro AccessorMacros.accessors
}

/**
 * Method Annotations
 */
class accessor extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro AccessorMacros.accessor
}

class getter extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro AccessorMacros.getter
}

class setter extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro AccessorMacros.setter
}

/**
 * All accessors can be called with the following arguments:
 * - `notNull`: Prevents wrapping the argument in Option, allows
 *              direct access to non nullable values.
 * - `varArgs`: Allows var args in the delegated setter.
 *
 *
 * TODO if there is a matching removeXYZ, then add None check to setter and
 *   use removeXYZ in case of None
 */
object AccessorMacros {

  /**
   * Invoked on a implicit class definition it queries all getters and
   * setters of the wrapped class and creates accessors for each of
   * them.
   *
   * Can also be called with `none` to prevent the generation of any
   * accessor
   */
  def accessors(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] =
    new AccessorClassMacros[c.type](c) { val anno = annottees }.accessors

  /** 
   * Invoked with an abstract method definition like `@accessor def foo: String`
   * this macro creates delegation methods for setters and getters on the pimped
   * object `self`.
   */
  def accessor(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = 
    new AnnotationToMacro[c.type](c) { val anno = annottees }.accessor
  
  /**
   * Invoked with an abstract method definition like `@getter def foo: String` 
   * this macro creates a degelation method for a getter on the pimped
   * object `self`
   */
  def getter(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = 
    new AnnotationToMacro[c.type](c) { val anno = annottees }.getter

  /**
   * Invoked with an abstract method definition like `@setter def foo: String` 
   * this macro creates a degelation method for a setter on the pimped
   * object `self`
   */
  def setter(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = 
    new AnnotationToMacro[c.type](c) { val anno = annottees }.setter
}

private[annotations] abstract class AnnotationToMacro [C <: Context](_c: C) extends AccessorMethodMacros[C](_c) {
  
  val anno: Seq[c.Expr[Any]]

  import c.universe._

  // Match on the annotation tree and extract necessary data
  lazy val List(Expr(DefDef(_, attrName, _, _, attrTypeTree, _))) = anno
  lazy val name: String = attrName.toString
  lazy val tpe: Type = toType(attrTypeTree)

  private val defaultArgs = Map('notNull -> false, 'varArgs -> false, 'none -> false)

  lazy val macroArgs =  defaultArgs ++ c.arguments.map((_, true)).toMap

  // validate arguments
  (macroArgs.keySet -- defaultArgs.keySet) match {
    case s if !s.isEmpty => c.abort(c.enclosingPosition, s"Illegal usage of not allowed flags: ${s.mkString(", ")}")
    case _ =>
  }

  def accessor = if (macroArgs('none)) assembleResult() else assembleResult((gen.getter ++ gen.setter):_*)
  def getter = assembleResult(gen.getter:_*)
  def setter = assembleResult(gen.setter:_*)
}

private[annotations] abstract class AccessorMethodMacros[C <: Context](val c: C) extends Utils[C] {

  import c.universe._

  val name: String
  val tpe: Type
  val macroArgs: Map[scala.Symbol, Boolean]

  object delegates {
    
    private lazy val getPrefix = tpe match {
      case _ if tpe =:= typeOf[Boolean] => "is"
      case _ => "get"
    }
    lazy val setter = newTermName("set" + name.capitalize)
    lazy val getter = newTermName(getPrefix + name.capitalize)
  }

  // Names that are used for the generated API
  object api {
    lazy val getter = newTermName(name)
    lazy val setter = newTermName(name + "_$eq")
  }

  // Templates
  object tpl {
    lazy val getter    = q"def ${api.getter} = Option(self.${delegates.getter})"
    lazy val getterNN  = q"def ${api.getter} = self.${delegates.getter}"
    lazy val setter    = q"def ${api.setter}(x: Option[$tpe]) { self.${delegates.setter}(x.orNull) }"
    lazy val setterNN  = q"def ${api.setter}(x: $tpe) { self.${delegates.setter}(x) }"
    lazy val setterNNV = q"def ${api.setter}(xs: Seq[$tpe]) { self.${delegates.setter}(xs:_*) }"
  }

  // Generators
  object gen {
    def getter: List[Tree] =
      if (macroArgs('notNull)) List(tpl.getterNN) else List(tpl.getter)

    def setter: List[Tree] = 
      if (macroArgs('varArgs))  {
        List(tpl.setterNNV)
      } else if (macroArgs('notNull)) {
        List(tpl.setterNN)
      } else {
        List(tpl.setter, tpl.setterNN)
      }
  }
}

private[annotations] abstract class AccessorClassMacros[C <: Context](val c: C) extends Utils[C] {

  val anno: Seq[c.Expr[Any]]

  import c.Expr
  import c.universe._

  /**
   * Only call with accessor methods like `get`, `set` or `is`
   */
  case class AccessorMethod(method: MethodSymbol) { self =>

    private lazy val _matched = ("^(is|get|set)(.)(.*)$".r findFirstMatchIn fullName).get
    private lazy val _macros = new AccessorMethodMacros[c.type](c) { 
      val name = self.name
      val tpe = self.tpe
      val macroArgs = Map(
        'notNull -> self.nonNullable,
        'varArgs -> method.hasVarArgs,
        'none -> false
      )
    }

    lazy val fullName: String = method.name.toString
    lazy val prefix: String = _matched.group(1)
    lazy val name: String = (_matched.subgroups: @unchecked) match {
      case prefix :: first :: rest :: Nil  => first.toLowerCase + rest
    }

    lazy val getter: Boolean = !setter
    lazy val setter: Boolean = prefix == "set"

    lazy val argCount: Int = method.paramss.map(_.size).sum

    lazy val tpe: Type = (setter, method.paramss) match {
      case (true, List(List(ts))) => {
        val t = ts.typeSignature

        if (method.hasVarArgs) 
          t.typeArguments.head
        else
          t
      }
      case (false, _) => method.returnType
    }

    // value types cannot be null and sequences are already functors
    lazy val nonNullable: Boolean = 
         (tpe <:< typeOf[AnyVal] ) || (tpe <:< typeOf[SeqLike[_,_]]) || (tpe <:< typeOf[Array[_]])

    def generate: List[Tree] = if (getter) _macros.gen.getter else _macros.gen.setter

    override def toString: String = s"$prefix $name: $tpe (nonNullable: $nonNullable)"
  }

  /**
   * Allows manual overwriting of accessors
   */
  def alreadyDefined(name: String, decls: List[Tree]): Boolean =  decls.exists { 
    case DefDef(_, methodName, _, _, _, _) => methodName.toString match {
      case n if n == name => true
      case n if n == s"${name}_=" => true
      case _ => false
    }
    case _ => false
  }

  def candidateMethods(cls: Type): List[AccessorMethod] = 
    for {
      member <- cls.members.toList
      if member.isMethod

      method = member.asMethod
      if !method.isDeprecated
      if !method.isOverridingSymbol
      if method.isPublic
      if method.isFirstOccurrenceIn(cls)
      if method.hasAccessorName

      accessor = AccessorMethod(method)
      if accessor.getter && accessor.argCount == 0 || 
         accessor.setter && accessor.argCount == 1
    } yield accessor


  lazy val result = anno.map(_.tree).toList match {
    case (param: ValDef) :: (enclosing: ClassDef) :: _ => { 
      val ClassDef(mods, name, tparams, Template(parents, self, body)) = enclosing
      val ValDef(_, _, tpt, _) = param

      val wrappedTpe = toType(tpt)

      val accessors = candidateMethods(wrappedTpe).filter { m => 
        !alreadyDefined(m.name, body)
      }.flatMap(_.generate)

      ClassDef(mods, name, tparams, Template(parents, self, body ++ accessors))
    }
  }

  def accessors = c.Expr(Block(List(result), Literal(Constant(()))))

}
