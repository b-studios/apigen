package apigen
package annotations

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.Context

class eventListener extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro EventListenerMacros.eventListener
}

object EventListenerMacros {

  /** 
   * Invoked with an abstract method definition like `@eventHandler def click: ClickHandler`
   * this macro creates the necessary facilities to register event handlers using a
   * lambda.
   * 
   * @example {{{
   *     def addClickListener(listener: ClickEvent => Unit) {
   *       self.addClickListener(new ClickListener {
   *         def buttonClick(evt: ClickEvent) = listener(evt)
   *       })
   *     }
   *     
   *     def click(listener: ClickEvent => Unit) { self.addClickListener(listener) }
   * }}}
   */
  def eventListener(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = 
    new EventListenerMacros[c.type](c) { val anno = annottees }.eventListener
}

abstract class EventListenerMacros[C <: Context](val c: C) extends Utils[C] {

  val anno: Seq[c.Expr[Any]]

  import c.universe._

  lazy val List(Expr(meth@DefDef(_, handlerName, _, _, handlerTypeTree, _))) = anno

  lazy val handlerTpe = toType(handlerTypeTree)

  object toImplement {

    private val abstractMethods = for {
      member <- handlerTpe.members
      if member.isMethod
      method = member.asMethod
      if method.isAbstract
    } yield method

    if (abstractMethods.size == 0) {
      c.abort(c.enclosingPosition, s"$handlerTypeTree has no abstract methods.")
    }

    if (abstractMethods.size > 1) {
      val methodList = abstractMethods.mkString("\n  ")
      c.abort(c.enclosingPosition, s"$handlerTypeTree has more then one abstract method. I cannot decide which one to use:\n  $methodList")
    }

    val method: MethodSymbol = abstractMethods.head

    val eventTpe: Type = method.paramss match {
      case List(List(p)) => p.typeSignature
    }
  }

  object names {
    lazy val method: TermName = newTermName(toImplement.method.name.toString)
    lazy val addListener = newTermName(s"add${handlerName.toString.capitalize}Listener")
    lazy val shorthand = newTermName(handlerName.toString)
  }

  lazy val addListener = q"""
    def ${names.addListener}(listener: ${toImplement.eventTpe} => Unit) {
      self.${names.addListener}(new $handlerTpe {
        def ${names.method}(evt: ${toImplement.eventTpe}) = listener(evt)
      })
    }
  """

  lazy val shortHand = q"""
    def ${names.shorthand}(listener: ${toImplement.eventTpe} => Unit) { 
      self.${names.addListener}(listener) 
    }"""

  def eventListener() = assembleResult(addListener, shortHand) 
}
