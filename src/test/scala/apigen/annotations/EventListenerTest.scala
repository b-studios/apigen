package apigen
package annotations

object EventListenerTest {

  trait SomeEvent {}

  trait SomeListener {
    def onFoo(e: SomeEvent): Unit
  }

  class A {
    def addSomeListener(l: SomeListener) {}
  }

  implicit class AWrapper(self: A) {
    @eventListener def some: SomeListener
  }

  locally {

    val a = new AWrapper(null)

    a.addSomeListener { evt => 
      println("some event happend!")
    }

    a.some { evt => 
      println("shorthand syntax works!")
    }
  }
}
