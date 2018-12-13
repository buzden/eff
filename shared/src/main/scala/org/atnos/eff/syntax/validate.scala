package org.atnos.eff.syntax

import cats.data.{NonEmptyList, ValidatedNel}
import org.atnos.eff._
import cats.Semigroup

object validate extends validate

trait validate {

  implicit class ValidateEffectOps[R, A](e: Eff[R, A]) {

    def runNel[E](implicit m: Member[Validate[E, ?], R]): Eff[m.Out, NonEmptyList[E] Either A] =
      ValidateInterpretation.runNel(e)(m.aux)

    def runMap[E, L : Semigroup](map: E => L)(implicit m: Member[Validate[E, ?], R]): Eff[m.Out, L Either A] =
      ValidateInterpretation.runMap(e)(map)(Semigroup[L], m.aux)

    def runValidatedNel[E](implicit m: Member[Validate[E, ?], R]): Eff[m.Out, ValidatedNel[E, A]] =
      ValidateInterpretation.runValidatedNel(e)(m.aux)

    def catchWrong[E](handle: E => Eff[R, A])(implicit m: Member[Validate[E, ?], R]): Eff[R, A] =
      ValidateInterpretation.catchWrong(e)(handle)

    def catchAllWrong[E](handle: NonEmptyList[E] => Eff[R, A])(implicit m: Member[Validate[E, ?], R]): Eff[R, A] =
      ValidateInterpretation.catchAllWrong(e)(handle)
  }

}
