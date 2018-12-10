package org.atnos.eff

import cats.Monoid
import cats.data._
import cats.implicits._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import org.specs2.{ScalaCheck, Specification}

class ValidateEffectSpec extends Specification with ScalaCheck { def is = s2"""

 run the validate effect                     $validateOk
 run the validate effect with nothing        $validateKo
 recover from wrong values                   $catchWrongValues1
 recover from wrong values and tell errors   $catchWrongValues2

 trivial catch preserves list of values      $idCatchPreserves
 catch should not change count of elements   $catchShouldNotChangeCount

 run is stack safe with Validate  $stacksafeRun

"""
  type S = Fx.fx1[ValidateString]

  def validateOk = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.correct[S, String, Int](2)
        a <- EffMonad[S].pure(3)
      } yield a

    validate.runNel.run ==== Right(3)
  }

  def validateKo = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error1")
        _ <- ValidateEffect.wrong[S, String]("error2")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.runNel.run ==== Left(NonEmptyList.of("error1", "error2"))
  }

  def catchWrongValues1 = {
    val validate: Eff[S, Int] =
      for {
        _ <- ValidateEffect.correct[S, String, Int](1)
        _ <- ValidateEffect.wrong[S, String]("error!")
        a <- EffMonad[S].pure(3)
      } yield a

    validate.catchWrong((s: String) => pure(4)).runNel.run ==== Right(4)
  }

  def catchWrongValues2 = {
    type E = String
    type Comput = Fx.fx2[Validate[E, ?], Writer[E,?]]
    type Check[A] = Eff[Comput, A]

    val handle: E => Check[Unit] = { case e => tell[Comput, E](e).as(()) }

    val comp1: Check[Int] = for {
      _ <- wrong[Comput, E]("1").catchWrong(handle)
      _ <- wrong[Comput, E]("2").catchWrong(handle)
    } yield 0

    val comp2: Check[Int] = comp1

    comp2.runNel.runWriter.run ==== ((Right(0), List("1", "2")))
  }

  def idCatchPreserves = {
    type S2 = Fx.fx2[ValidateString, List]
    type _validateString[R] = ValidateString |= R

    def v[R: _validateString:_list](xs: List[Int]): Eff[R, Int] = for {
      x <- fromList(xs)
      y <- validateValue[R, String, Int](x > 0, x + 1, s"$x is too small")
      _ <- validateValue[R, String, Int](x % 2 === 0, x + 1, s"$x is too odd")
    } yield y

    /** Just rethrows an error. Monoid is needed only for the pseudo-coercion. */
    def idCatch[E, A: Monoid, R: Validate[E, ?] |= ?](e: E): Eff[R, A] = wrong(e) >> Eff.pure(Monoid[A].empty)

    prop { l: List[Int] =>
      val original = v[S2](l)
      val rethrown = v[S2](l).catchWrong { e: String => idCatch[String, Int, S2](e) }

      rethrown.runList.runNel.run === original.runList.runNel.run
    }
  }

  def catchShouldNotChangeCount = {
    type ValidateInt[A] = Validate[Int, A]
    type S2 = Fx.fx2[ValidateInt, List]
    type _validateInt[R] = ValidateInt |= R

    def v[R: _validateInt:_list](xs: List[Int]): Eff[R, Int] = for {
      x <- fromList(xs)
      _ <- wrong(x + 100)
      _ <- wrong(x + 101)
    } yield x

    prop { l: List[Int] =>
      val eith = v[S2](l).catchWrong{ e: Int => correct(e) }.runList.runNel.run
      eith.map(_.size) === Right(l.size)
    }
  }

  type ValidateString[A] = Validate[String, A]

  def stacksafeRun = {
    val list = (1 to 5000).toList
    val action = list.traverse(i => ValidateEffect.wrong[S, String](i.toString))

    action.runNel.run must not(throwAn[Exception])
  }

}

