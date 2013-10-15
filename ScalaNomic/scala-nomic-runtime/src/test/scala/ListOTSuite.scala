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

class ListOTSuite extends Assertions{
  val translator = new Translator()
  val ot = new IdentityListOT[Int]
  val obj= List(1,2,3)

  @Test def testIdentity() {
    assert(ot(translator, obj) === obj)
  }
  @Test def testJoin() {
    assert(ot.join.indent.apply(translator, obj) === "  123")
    assert(ot.join(", ").indent.apply(translator, obj) === "  1, 2, 3")
    assert(ot.join(ot.C(", ")).indent.apply(translator, obj) === "  1, 2, 3")
  }
  @Test def testMap() {
    val id = new IdentityOT[Int]
    assert(ot.map(id.str + id.str).join(",").indent.apply(translator, obj) === "  11,22,33")
  }
  @Test def testMapLambda() {
    assert(ot.map(_ + 1).join.indent.apply(translator, obj) === "  234")
  }
  @Test def testFilter() {
    val id = new IdentityOT[Int]
    assert(ot.filter(id/(_ > 1)).join(",").indent.apply(translator, obj) === "  2,3")
  }
  @Test def testFilterLambda() {
    assert(ot.filter(_ > 1).join(",").indent.apply(translator, obj) === "  2,3")
  }
}