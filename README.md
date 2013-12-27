APIGen
======
APIGen is a little helper designed to generate a "scalaish" wrapper around an already
existing Java API.

*DISCLAIMER: Large parts of APIGen are based on experimental scala features. Use on your own risk*


Design Rationale
----------------
The design rationale is based on the following principles:

- few boilerplate as possible
- full backwards compatibility to the original Java API
- low maintenance effort for *your* Scala API

APIGen is implemented as scala macros, that create wrapper classes following the
pimp my library pattern[1].


What APIGen can do for you
--------------------------
Given the following simple Java interface:
~~~Java
public interface Person {
  public String getName();
  public void setName(String name);
  public int getAge();
  public void setAge(int age);
  public boolean isCool();
  public void setCool(boolean cool);
}
~~~

You can create your own wrapper class using APIGen:
~~~Scala
implicit class PersonWrapper(@accessors self: Person) {}
~~~

By simply putting `@accessors` in front of the wrapped instance, getters and setters
are generated and can be used as in the following example;
~~~Scala
val me: Person = ...
me.name // Option[String]
me.name = Some("Jonathan")
me.name = "Jonathan" // Also works
me.age // Int
me.cool // Boolean
~~~

Since `String` is a reference type it can be null and thus is wrapped into `Option`. If we
know better, we can change this behaviour by manually specifying the accessor for `name`:
~~~Scala
implicit class PersonWrapper(@accessors self: Person) {
  @accessor(notNull) def name: String
}
~~~

The flag `notNull` prevents wrapping and unwrapping nullable references in `Option`.

Generation of a field can be prevented alltogether by adding the `none` flag as in `@accessor(none) def age`.

More examples can be found in `src/test/scala/apigen/annotations`.

[1]: http://www.artima.com/weblogs/viewpost.jsp?thread=179766
