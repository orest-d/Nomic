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
import eu.lateral.nomic.TextUtils._

class TextUtilsSuite extends Assertions {
  val texts=List("testThisText","test_ThisText","TEST_THIS_TEXT","test-this,text")
  val cap="CAp_Text"

  @Test def testIsAllCapital() {
     assert(isAllCapital("XY"))
     assert(!isAllCapital("xY"))
  }
    
  @Test def testIdToList() {
    assert(idToList("").length === 0)
    assert(idToList("")===List())
    assert(idToList(texts(0))===List("test","This","Text"))
    assert(idToList(texts(1))===List("test","This","Text"))
    assert(idToList(texts(2))===List("TEST","THIS","TEXT"))
    assert(idToList(texts(3))===List("test","this","text"))
    assert(idToList(cap)===List("C","Ap","Text"))
  }
  
  @Test def testUpperCamelCase(){
    for(t <- texts){
      assert(upperCamelCase(t) === "TestThisText")
    }
    assert(upperCamelCase("")==="")
    assert(upperCamelCase(cap)==="CApText")
  }

    @Test def testLowerCamelCase(){
    for(t <- texts){
      assert(lowerCamelCase(t) === "testThisText")
    }
    assert(lowerCamelCase("")==="")
    assert(lowerCamelCase(cap)==="cApText")
  }
  @Test def testAllCaps(){
    for(t <- texts){
      assert(allCaps(t) === "TEST_THIS_TEXT")
    }
    assert(allCaps("")==="")
    assert(allCaps(cap)==="C_AP_TEXT")
  }
  @Test def testAllCapsSeparated(){
    for(t <- texts){
      assert(allCaps(t,"::") === "TEST::THIS::TEXT")
    }
    assert(allCaps("","::")==="")
    assert(allCaps(cap,"::")==="C::AP::TEXT")
  }
  @Test def testSmallCaps(){
    for(t <- texts){
      assert(smallCaps(t) === "test_this_text")
    }
    assert(smallCaps("")==="")
    assert(smallCaps(cap)==="c_ap_text")
  }
  @Test def testSmallCapsSeparated(){
    for(t <- texts){
      assert(smallCaps(t,"::") === "test::this::text")
    }
    assert(smallCaps("","::")==="")
    assert(smallCaps(cap,"::")==="c::ap::text")
  }
  @Test def testJoinText() {
    assert(joinText("")==="")
    assert(joinText(texts(0))==="test This Text")
    assert(joinText(texts(1))==="test This Text")
    assert(joinText(texts(2))==="TEST THIS TEXT")
    assert(joinText(texts(3))==="test this text")
    assert(joinText(cap)==="C Ap Text")
  }
  @Test def testJoinTextSeparated() {
    assert(joinText("","::")==="")
    assert(joinText(texts(0),"::")==="test::This::Text")
    assert(joinText(texts(1),"::")==="test::This::Text")
    assert(joinText(texts(2),"::")==="TEST::THIS::TEXT")
    assert(joinText(texts(3),"::")==="test::this::text")
    assert(joinText(cap,"::")==="C::Ap::Text")
  }
  @Test def testIndent(){
    assert(indent("")==="")
    assert(indent("a\n b\nc")==="  a\n   b\n  c")
    assert(indent("a\n b\nc",2)==="    a\n     b\n    c")
    assert(indent("a\n b\nc\n")==="  a\n   b\n  c\n")
    assert(indent("a\n b\nc\n","***")==="***a\n*** b\n***c\n")
  }
  @Test def testExactSplit(){
    assert(exactSplit("",':')===List(""))
    assert(exactSplit("a:bb",':')===List("a","bb"))
    assert(exactSplit("a:bb:",':')===List("a","bb",""))
  }
  @Test def testEscape(){
    assert(escape("")==="")
    assert(escape("a\nb")==="a\\nb")
    assert(escape("a\rb")==="a\\rb")
    assert(escape("a\tb")==="a\\tb")
    assert(escape("a\\b")==="a\\\\b")
    assert(escape("a\"b")==="a\\\"b")
    assert(escape("a\'b")==="a\\\'b")
  }
  
}

