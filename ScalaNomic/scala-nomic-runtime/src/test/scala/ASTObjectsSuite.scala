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

class ASTObjectsSuite extends Assertions {
  @Test def testLiteral() {
    val obj = new Literal("xyz")
    assert(obj.literal === "xyz")
    assert(obj.toString === "Literal(xyz)")
    assert(obj.parent === None)
    assert(obj.ancestor[Literal] === None)
    assert(obj.pos.toString() === "<undefined position>")
    assert(obj.get_children.length === 0)
    assert(obj.get_topDown.length === 1)
    assert(obj.get_topDown.head === obj)
    assert(obj.get_bottomUp.length === 1)
    assert(obj.get_bottomUp.head === obj)
  }

  @Test def testAGroup() {
    val l=new Literal("xyz")
    val obj = new AGroup(l)
    assert(obj.content.toString === "Literal(xyz)")
    assert(obj.toString === "AGroup(Literal(xyz))")
    assert(obj.content.parent === Some(obj))
    assert(obj.parent === None)
    assert(obj.get_children.length === 1)
    assert(obj.get_topDown.length === 2)
    assert(obj.get_topDown.head === obj)
    assert(obj.get_topDown(1) === l)
    assert(obj.get_bottomUp.length === 2)
    assert(obj.get_bottomUp.head === l)
    assert(obj.get_bottomUp(1) === obj)
  }

  @Test def testAGroupAncestor() {
    val l=new Literal("xyz")
    val obj = new AGroup(l)
    assert(obj.content.ancestor[AGroup] === Some(obj))
    assert(obj.content.ancestor[Literal] === None)
    assert(obj.get_children.length === 1)
    assert(obj.get_topDown.length === 2)
    assert(obj.get_topDown.head === obj)
    assert(obj.get_topDown(1) === l)
    assert(obj.get_bottomUp.length === 2)
    assert(obj.get_bottomUp.head === l)
    assert(obj.get_bottomUp(1) === obj)
  }

  @Test def testAList() {
    val a = new Literal("a")
    val b = new Literal("b")
    val obj = new AList(List(a,b))
    assert(obj.toString === "List(Literal(a), Literal(b))")
    assert(obj.get_children.length === 2)
    assert(obj.get_children(0) === a)
    assert(obj.get_children(1) === b)
    assert(obj.get_topDown.length === 3)
    assert(obj.get_topDown(0) === obj)
    assert(obj.get_topDown(1) === a)
    assert(obj.get_topDown(2) === b)
    assert(obj.get_bottomUp.length === 3)
    assert(obj.get_bottomUp(0) === a)
    assert(obj.get_bottomUp(1) === b)
    assert(obj.get_bottomUp(2) === obj)
  }

  @Test def testABinary() {
    val a = new Literal("a")
    val b = new Literal("b")
    val obj = new ABinary(a,b)
    assert(obj.toString === "Binary(Literal(a), Literal(b))")
    assert(obj.get_children.length === 2)
    assert(obj.get_children(0) === a)
    assert(obj.get_children(1) === b)
    assert(obj.get_topDown.length === 3)
    assert(obj.get_topDown(0) === obj)
    assert(obj.get_topDown(1) === a)
    assert(obj.get_topDown(2) === b)
    assert(obj.get_bottomUp.length === 3)
    assert(obj.get_bottomUp(0) === a)
    assert(obj.get_bottomUp(1) === b)
    assert(obj.get_bottomUp(2) === obj)
  }
}