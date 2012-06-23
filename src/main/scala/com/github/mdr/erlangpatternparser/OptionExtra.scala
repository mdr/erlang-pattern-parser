package com.github.mdr.erlangpatternparser

object OptionExtra {

  implicit def option2OptionExtra[T](option: Option[T]): OptionExtra[T] = new OptionExtra[T](option)
  implicit def boolean2BooleanExtra(boolean: Boolean): BooleanExtra = new BooleanExtra(boolean)

  def nullableToOption[T](nullable: T): Option[T] = (nullable != null) thenSome nullable

  def maybeNegativeToOption(n: Int): Option[Int] = (n >= 0) thenSome n

  def foldM[A, B](f: (A, B) ⇒ Option[A], a: A, lst: List[B]): Option[A] =
    lst match {
      case Nil ⇒ Some(a)
      case x :: xs ⇒ for (y ← f(a, x); result ← foldM(f, y, xs))
        yield result
    }

  def safeCast[T](x: Object, klass: Class[T]): Option[T] = klass.isAssignableFrom(x.getClass) thenSome x.asInstanceOf[T]

}

class OptionExtra[T](val option: Option[T]) {

  def when(condition: Boolean) = option.filter(_ ⇒ condition)

}

class BooleanExtra(val boolean: Boolean) {

  def thenSome[T](item: ⇒ T) = if (boolean) Some(item) else None

  def then[T](opt: Option[T]) = if (boolean) opt else None

}
