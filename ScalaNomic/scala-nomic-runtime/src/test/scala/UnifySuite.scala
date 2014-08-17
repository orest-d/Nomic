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
import eu.lateral.nomic.unify._
import org.junit.Test
import eu.lateral.nomic.ASTObjects._

class UnifySuite extends Assertions {
  @Test def testGetTyped() {
    val d = new Context
    val d1 = d + ("a" -> "abc")
    assert(d1.getTyped[Int]("a") === None)
    assert(d1.getTyped[String]("a") === Some("abc"))
  }

  @Test def testVariableUnification() {
    val v = SimpleVariable("a")
    val result = v.unify(SimpleUnifiable("abc"), new Context)
    val contexts = result.contexts
    assert(contexts.length === 1)
    assert(contexts.head.getTyped[SimpleUnifiable[String]]("a") === Some(SimpleUnifiable("abc")))
  }
  
  @Test def testGetBound1() {
    val a = SimpleVariable("a")
    val b = SimpleVariable("b")

    val r = for (
      c1 <- a.unify(SimpleUnifiable("abc"), new Context);
      c2 <- b.unify(a, c1)
    ) yield c2
    assert(r.contexts.length === 1)
    val c = r.contexts.head
    assert(c("a") === SimpleUnifiable("abc"))
    assert(c("b") === a)
    assert(a.get_bound(c) === Some(SimpleUnifiable("abc")))
    assert(b.get_bound(c) === Some(SimpleUnifiable("abc")))
  }
  
  @Test def testGetBound2() {
    val a = SimpleVariable("a")
    val b = SimpleVariable("b")

    val r = for (
      c1 <- a.unify(SimpleUnifiable("abc"), new Context);
      c2 <- a.unify(b, c1)
    ) yield c2
    assert(r.contexts.length === 1)
    val c = r.contexts.head
    assert(c("a") === SimpleUnifiable("abc"))
    assert(c("b") === SimpleUnifiable("abc"))
    assert(a.get_bound(c) === Some(SimpleUnifiable("abc")))
    assert(b.get_bound(c) === Some(SimpleUnifiable("abc")))
  }
  
  @Test def testGetBoundTransitive() {
    val a = SimpleVariable("a")
    val b = SimpleVariable("b")
    val c = SimpleVariable("c")

    val r = for (
      c1 <- a.unify(SimpleUnifiable("abc"), new Context);
      c2 <- b.unify(a, c1);
      c3 <- c.unify(b, c2)
    ) yield c3
    assert(r.contexts.length === 1)
    val context = r.contexts.head
    assert(a.get_bound(context) === Some(SimpleUnifiable("abc")))
    assert(b.get_bound(context) === Some(SimpleUnifiable("abc")))
    assert(c.get_bound(context) === Some(SimpleUnifiable("abc")))
  }

  @Test def testOrResult() {
    val a = SimpleVariable("a")

    val res=new OrResult(new Context + ("a" -> SimpleUnifiable("A")))(
      new OrResult(new Context + ("a" -> SimpleUnifiable("B")))(Success(new Context + ("a" -> SimpleUnifiable("C"))))
    )
    val r = for (c <- res) yield c
    assert(r.contexts.length === 3)
    assert(r.contexts.map(a.get_bound _).flatten.toList === List(SimpleUnifiable("A"),SimpleUnifiable("B"),SimpleUnifiable("C")))
  }
  
}
