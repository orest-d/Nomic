/*
This file is part of Scala Nomic Runtime.

    Scala Nomic Runtime is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scala Nomic Runtime is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Scala Nomic Runtime.  If not, see <http://www.gnu.org/licenses/>.
*/

import org.scalatest.Assertions
import org.junit.Test
import eu.lateral.nomic.ASTObjects._
import eu.lateral.nomic.ObjectTranslators._

class BooleanOTSuite extends Assertions{
  val translator = new Translator()

  @Test def testIdentity() {
    assert(BooleanOT(translator, true) === true)
    assert(BooleanOT(translator, false) === false)
  }
  @Test def testNot() {
    assert(!BooleanOT(translator, true) === false)
    assert(!BooleanOT(translator, false) === true)
  }
  @Test def testAnd() {
    val t = BooleanOT && BooleanOT.C(true)
    val f = BooleanOT && BooleanOT.C(false)
    assert(t (translator, true) === true)
    assert(t (translator, false) === false)
    assert(f (translator, true) === false)
    assert(f (translator, false) === false)
  }
  @Test def testOr() {
    val t = BooleanOT || BooleanOT.C(true)
    val f = BooleanOT || BooleanOT.C(false)
    assert(t (translator, true) === true)
    assert(t (translator, false) === true)
    assert(f (translator, true) === true)
    assert(f (translator, false) === false)
  }
  @Test def testIfTrueElse() {
    val ot = BooleanOT.ifTrueElse(1,2)
    assert(ot(translator, true) === 1)
    assert(ot(translator, false) === 2)
  }
  @Test def testIfTrueElseOT() {
    val ot = BooleanOT.ifTrueElse(BooleanOT.C(1),BooleanOT.C(2))
    assert(ot(translator, true) === 1)
    assert(ot(translator, false) === 2)
  }
  @Test def testIfTrueElseString() {
    val ot = BooleanOT.ifTrueElse("yes","no").toUpperCase
    assert(ot(translator, true) === "YES")
    assert(ot(translator, false) === "NO")
  }
  @Test def testEquals() {
    val ot=(StringOT === StringOT.C("x")).ifTrueElse("yes","no").toUpperCase
    assert(ot(translator, "x") === "YES")
    assert(ot(translator, "y") === "NO")
  }

}