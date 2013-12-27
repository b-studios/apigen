package apigen
package annotations

object AccessorTest {

  trait A {
    def getFoo: Int = 42
    def setFoo(n: Int) {}
    def getBar: String = "hello"
    def setBar(s: String) {}
    def isBoo: Boolean = true
    def getBaz: String
    def setVarArg(a: String*) {}
    def getVarArg: Seq[String] = Seq("Hello", "World")
  }

  trait B extends A {
    private def getA: A = ???
    def getFoo(n: Int): String = n.toString
    def setBam = 42
    def getBaz = "world"
  }

  locally {
    implicit class AWrapper(@accessors self: A) {}

    val a = new AWrapper(null)

    // foo is non nullable
    val foo: Int = a.foo
    a.foo = 42

    // bar is nullable (since it's a reference type)
    val bar: Option[String] = a.bar
    a.bar = "Hello"
    a.bar = Some("Hello")
 
    // abstract method
    val baz: String = a.baz getOrElse ""

    // methods with "is"-prefix
    val boo: Boolean = a.boo

    // varargs - have to be applied as setParentMethod(foo: _*)
    val varargs: Seq[String] = a.varArg
    a.varArg = List("foo")
  }

  // it should not generate accessors if they are specified manually
  locally {
    implicit class AWrapper(@accessors self: A) {
      @accessor(none) def foo
      @getter(notNull) def bar: String
    }

    val a = new AWrapper(null)
    val bar: String = a.bar
    illTyped("a.foo", "value foo is not a member of AWrapper")
  }

  // it should not generate accessors for 
  // - private members
  // - inherited members
  // - members with wrong argument pattern
  locally {
    implicit class BWrapper(@accessors self: B) {}
  
    val b = new BWrapper(null) {}
    
    illTyped("b.a", "value a is not a member of BWrapper")
    illTyped("b.bar", "value bar is not a member of BWrapper")
    illTyped("b.foo(42)", "value foo is not a member of BWrapper")
    illTyped("b.baz", "value baz is not a member of BWrapper")
  }


  trait P[T] {
    def getT: T
    def setT(t: T): Unit
  }
  // Does not work:
  // locally {
  //   // at macro expansion time, I don't have information whether T can be nullable
  //   // or not (except there is an upper bound)
  //   implicit class PWrapper[T](@accessors self: P[_]) {}
  //   val p = new PWrapper[Int](null)
  //   val t: Int = p.t
  // }

  
  // Does not work:  
  // locally {
  //   type C = A // Moving the type alias outside of `locally` works
  //   implicit class AWrapper(@accessors self: C) {}
  //   val a = new AWrapper(null)
  //   a.foo
  // }
}
