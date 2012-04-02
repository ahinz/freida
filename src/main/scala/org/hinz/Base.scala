package org.hinz

object base {

  def id[A](a: A) = a

  // Typeclasses
  trait Showable[T] {
    def asString(t:T): String
  }

  trait Eq[T] {
    def isEqual(t1: T, t2: T): Boolean
  }

  def asString[T](obj: T)(implicit s:Showable[T]):String = s.asString(obj)

  def show[T](obj: T)(implicit s:Showable[T]):Unit = {
    println(s.asString(obj))
  }

  implicit def productIsShowable[T <: Product]: Showable[T] = new Showable[T] { def asString(t: T) = t.toString }
  implicit def stringIsShowable: Showable[String] = new Showable[String] { def asString(t: String) = t }
  implicit def anyValIsShowable[T <: AnyVal]: Showable[T] = new Showable[T] { def asString(t: T) = "" + t }
  implicit def mapIsShowable[X:Showable,Y:Showable]: Showable[Map[X,Y]] = new Showable[Map[X,Y]] {
    def pad(s: String, len: Int) = {
      if (s.length() >= len) {
        s
      } else {
        s + rep(len - s.length())
      }
    }

    def rep(z: Int):String = (1 to z).map(z => " ").mkString("")
      

    def asString(map: Map[X,Y]) = {
      val showKey = implicitly[Showable[X]]
      val showVal = implicitly[Showable[Y]]

      val shown:Map[String,String] = map.map (tpl => (showKey.asString(tpl._1), showVal.asString(tpl._2)))
      val maxKeyLen = shown.map (tpl => tpl._1.length) max

      val content = shown.map (tpl => ("  %s -> %s" format (pad(tpl._1, maxKeyLen), tpl._2))).mkString("\n")
      "Map[length=%d] {\n%s\n}" format (map.size, content)
    }
  }
}
