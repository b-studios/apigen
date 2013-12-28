package apigen
package standalone

trait A { def foo: Int }

trait P[T] { def foo: T }

object test {
  
  // works
  implicit class AWrapper(@accessors self: A) {}

  type B = A

  // does not work: "scala.reflect.macros.TypecheckException: not found: type B"
  // implicit class BWrapper(@accessors self: B) {}

  // works again
  locally {
    implicit class BWrapper(@accessors self: B) {}
  }

  // does not work: "scala.reflect.macros.TypecheckException: not found: type T"
  // implicit class PWrapper[T](@accessors self: P[T]) {}

}