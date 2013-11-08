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

class ASTObjectsSuite extends Assertions{
  @Test def testLiteral() {
     val obj = new Literal("xyz")
     assert(obj.literal==="xyz")
     assert(obj.toString==="Literal(xyz)")
     assert(obj.parent === None)
     assert(obj.ancestor[Literal] === None)
     assert(obj.pos.toString() === "<undefined position>")
  }
  @Test def testAGroup() {
     val obj = new AGroup(new Literal("xyz"))
     assert(obj.groupContent.toString==="Literal(xyz)")
     assert(obj.toString==="AGroup(Literal(xyz))")
     assert(obj.groupContent.parent === Some(obj))
     assert(obj.parent === None)
  }
  @Test def testAGroupAncestor() {
     val obj = new AGroup(new Literal("xyz"))
     assert(obj.groupContent.ancestor[AGroup] === Some(obj))
     assert(obj.groupContent.ancestor[Literal] === None)
  }
  @Test def testAList() {
    val obj= new AList(List(new Literal("a"),new Literal("b")))
    assert(obj.toString==="List(Literal(a), Literal(b))")
  }
  @Test def testABinary() {
    val obj= new ABinary(new Literal("a"),new Literal("b"))
    assert(obj.toString==="Binary(Literal(a), Literal(b))")
  }
}