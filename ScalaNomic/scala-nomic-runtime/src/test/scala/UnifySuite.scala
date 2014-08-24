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
    val d1 = d + ("a" -> SimpleUnifiable("abc"))
    assert(d1.getTyped[Int]("a") === None)
    assert(d1.getTyped[SimpleUnifiable[String]]("a") === Some(SimpleUnifiable("abc")))
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
  @Test def testUnifySelf() {
    val a1 = SimpleVariable("a")
    val a2 = SimpleVariable("a")
    val res = a1.unify(a2,Context())
    assert(res.success == true)
    assert(res.contexts.length === 1)
    assert(res.contexts.head.size === 0)
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

  @Test def testOrResultWithFilter() {
    val a = SimpleVariable("a")

    val res=new OrResult(new Context + ("a" -> SimpleUnifiable("A")))(
      new OrResult(new Context + ("a" -> SimpleUnifiable("B")))(Success(new Context + ("a" -> SimpleUnifiable("C"))))
    )
    val r = for (
      c <- res;
      if (c("a") != SimpleUnifiable("B"))
    ) yield c
    assert(r.contexts.length === 2)
    assert(r.contexts.map(a.get_bound _).flatten.toList === List(SimpleUnifiable("A"),SimpleUnifiable("C")))
  }
  
  @Test def testUnifyListsNilNil() {
    val res=Unify.unifyLists(Nil,Nil,Context())
    assert(res.success === true)
  }
  
  @Test def testUnifyListsNilA() {    
    val res1=Unify.unifyLists(Nil,List(SimpleUnifiable("A")),Context())
    assert(res1.success === false)
    val res2=Unify.unifyLists(List(SimpleUnifiable("A")),Nil,Context())
    assert(res2.success === false)
  }
  
  @Test def testUnifyListsAA() {    
    val res1=Unify.unifyLists(List(SimpleUnifiable("A")),List(SimpleUnifiable("A")),Context())
    assert(res1.success === true)
    
    val res2=Unify.unifyLists(List(SimpleUnifiable("A")),List(SimpleUnifiable("B")),Context())
    assert(res2.success === false)
  }
  
  @Test def testUnifyListsWithVariable() {    
    val res=Unify.unifyLists(List(SimpleUnifiable("A")),List(SimpleVariable("a")),Context())
    assert(res.success === true)
    assert(res.contexts.length === 1)
    val context = res.contexts.head
    assert(context.getResolved("a") === Some(SimpleUnifiable("A")))
  }

  @Test def testUnifyOptionsNoneNone() {
    val res=Unify.unifyOptions(None,None,Context())
    assert(res.success === true)
  }
  
  @Test def testUnifyOptionsNoneA() {    
    val res1=Unify.unifyOptions(None,Some(SimpleUnifiable("A")),Context())
    assert(res1.success === false)
    val res2=Unify.unifyOptions(Some(SimpleUnifiable("A")),None,Context())
    assert(res2.success === false)
  }
  
  @Test def testUnifyOptionsAA() {    
    val res1=Unify.unifyOptions(Some(SimpleUnifiable("A")),Some(SimpleUnifiable("A")),Context())
    assert(res1.success === true)
    
    val res2=Unify.unifyOptions(Some(SimpleUnifiable("A")),Some(SimpleUnifiable("B")),Context())
    assert(res2.success === false)
  }
  
  @Test def testUnifyOptionsWithVariable() {    
    val res=Unify.unifyOptions(Some(SimpleUnifiable("A")),Some(SimpleVariable("a")),Context())
    assert(res.success === true)
    assert(res.contexts.length === 1)
    val context = res.contexts.head
    assert(context.getResolved("a") === Some(SimpleUnifiable("A")))
  }  
  
  @Test def testUnifyBooleans() {    
    assert(Unify.unifyBooleans(true,true,Context()).success === true)
    assert(Unify.unifyBooleans(false,false,Context()).success === true)
    assert(Unify.unifyBooleans(true,false,Context()).success === false)
    assert(Unify.unifyBooleans(false,true,Context()).success === false)
  }  

  @Test def testPrimitiveFilter() {
    val res1 = for (c <- Success();if(true)) yield c
    assert(res1.success === true)
    val res2 = for (c <- Success();if(false)) yield c
    assert(res2.success === false)
  }  
}
