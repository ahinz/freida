package org.hinz

import org.hinz.base._

object validation {

  sealed abstract class Validation[T] {
    def map[Q](f: T => Q):Validation[Q] = flatMap(t => Valid(f(t)))
    def flatMap[Q](f: T => Validation[Q]):Validation[Q] 
    def >>=[Q](f: T => Validation[Q]):Validation[Q] = flatMap(f)

    def isValid():Boolean
    def appendError(err: String): Validation[T]
  }

  case class Valid[T](val t: T) extends Validation[T] {
    def flatMap[Q](f: T => Validation[Q]) = f(t)

    def isValid() = true
    def appendError(err: String) = Error(List(err))
  }

  case class Error[T](val errors:List[String]) extends Validation[T] {
    def flatMap[Q](f: T => Validation[Q]) = Error[Q](errors)
    def isValid() = false
    def appendError(err: String) = Error(err :: errors)
  }

  implicit def ValidationIsShowable[T:Showable] = new Showable[Validation[T]] {
    def asString(v: Validation[T]) = v match {
      case vv@Valid(t) => implicitly[Showable[Valid[T]]].asString(vv)
      case err@Error(errors) => implicitly[Showable[Error[T]]].asString(err)
    }
  }

}
