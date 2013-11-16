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

class RichStringSuite extends Assertions {
  val texts=List("testThisText","test_ThisText","TEST_THIS_TEXT","test-this,text")
  val cap="CAp_Text"

  @Test def testIsAllCapital() {
     assert("XY".isAllCapital)
     assert(!"xY".isAllCapital)
  }
    
  @Test def testIdToList() {
    assert("".idToList.length === 0)
    assert("".idToList===List())
    assert(texts(0).idToList===List("test","This","Text"))
    assert(texts(1).idToList===List("test","This","Text"))
    assert(texts(2).idToList===List("TEST","THIS","TEXT"))
    assert(texts(3).idToList===List("test","this","text"))
    assert(cap.idToList===List("C","Ap","Text"))
  }
  
  @Test def testUpperCamelCase(){
    for(t <- texts){
      assert(t.upperCamelCase === "TestThisText")
    }
    assert("".upperCamelCase==="")
    assert(cap.upperCamelCase==="CApText")
  }

    @Test def testLowerCamelCase(){
    for(t <- texts){
      assert(t.lowerCamelCase === "testThisText")
    }
    assert("".lowerCamelCase==="")
    assert(cap.lowerCamelCase==="cApText")
  }
  @Test def testAllCaps(){
    for(t <- texts){
      assert(t.allCaps === "TEST_THIS_TEXT")
    }
    assert("".allCaps==="")
    assert(cap.allCaps==="C_AP_TEXT")
  }
  @Test def testAllCapsSeparated(){
    for(t <- texts){
      assert(t.allCaps("::") === "TEST::THIS::TEXT")
    }
    assert("".allCaps("::")==="")
    assert(cap.allCaps("::")==="C::AP::TEXT")
  }
  @Test def testSmallCaps(){
    for(t <- texts){
      assert(t.smallCaps === "test_this_text")
    }
    assert("".smallCaps==="")
    assert(cap.smallCaps==="c_ap_text")
  }
  @Test def testSmallCapsSeparated(){
    for(t <- texts){
      assert(t.smallCaps("::") === "test::this::text")
    }
    assert("".smallCaps("::")==="")
    assert(cap.smallCaps("::")==="c::ap::text")
  }
  @Test def testJoinText() {
    assert("".joinText==="")
    assert(texts(0).joinText==="test This Text")
    assert(texts(1).joinText==="test This Text")
    assert(texts(2).joinText==="TEST THIS TEXT")
    assert(texts(3).joinText==="test this text")
    assert(cap.joinText==="C Ap Text")
  }
  @Test def testJoinTextSeparated() {
    assert("".joinText("::")==="")
    assert(texts(0).joinText("::")==="test::This::Text")
    assert(texts(1).joinText("::")==="test::This::Text")
    assert(texts(2).joinText("::")==="TEST::THIS::TEXT")
    assert(texts(3).joinText("::")==="test::this::text")
    assert(cap.joinText("::")==="C::Ap::Text")
  }
  @Test def testIndent(){
    assert("".indent==="")
    assert("a\n b\nc".indent==="  a\n   b\n  c")
    assert("a\n b\nc".indent(2)==="    a\n     b\n    c")
    assert("a\n b\nc\n".indent==="  a\n   b\n  c\n")
    assert("a\n b\nc\n".indent("***")==="***a\n*** b\n***c\n")
  }
  @Test def testExactSplit(){
    assert("".exactSplit(':')===List(""))
    assert("a:bb".exactSplit(':')===List("a","bb"))
    assert("a:bb:".exactSplit(':')===List("a","bb",""))
  }
  @Test def testEscape(){
    assert("".escape==="")
    assert("a\nb".escape==="a\\nb")
    assert("a\rb".escape==="a\\rb")
    assert("a\tb".escape==="a\\tb")
    assert("a\\b".escape==="a\\\\b")
    assert("a\"b".escape==="a\\\"b")
    assert("a\'b".escape==="a\\\'b")
  }
  
}

