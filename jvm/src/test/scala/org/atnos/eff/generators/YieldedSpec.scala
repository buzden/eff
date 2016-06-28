package org.atnos.eff.generators

import org.specs2._
import Yielded._
import org.atnos.eff._, all._, syntax.all._

class YieldedSpec extends Specification with ScalaCheck { def is = s2"""

  emit / collect $emitCollect

  emit / filter / collect $emitFilterCollect

"""

  def emitCollect = prop { xs: List[Int] =>
    collect[NoEffect, Int](emit(xs)).run ==== xs
  }

  def emitFilterCollect = prop { xs: List[Int] =>
    collect[NoEffect, Int](filter(emit(xs))(_ > 2)).run ==== xs.filter(_ > 2)
  }

}