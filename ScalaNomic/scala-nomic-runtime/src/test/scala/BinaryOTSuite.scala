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

class BinaryOTSuite extends Assertions{
  val translator = new Translator()

  @Test def testIdentity() {
    type B = ABinary[Literal]
    type LOT = LiteralOT[B,Literal]
    val a = new Literal("a")
    val b = new Literal("b")
    val binary:B = new ABinary(a,b)
    val identity = new IdentityOT[B]
    val ot = new BinaryOT[B,B,Literal,LOT](identity,x => new LiteralOT(x))
    assert(ot(translator,binary) === binary)
    assert(ot.left.value.toUpperCase(translator,binary) === "A")
    assert(ot.right.value.toUpperCase(translator,binary) === "B")
  }


}