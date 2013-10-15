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

class StringOTSuite extends Assertions {
  val obj = "xyz_Abc"
  val translator = new Translator()

  @Test def testIdentity() {
    assert(StringOT(translator, obj) === "xyz_Abc")
  }
  @Test def testToLowerCase() {
    assert(StringOT.toLowerCase.apply(translator, obj) === "xyz_abc")
  }
  @Test def testToUpperCase() {
    assert(StringOT.toUpperCase.apply(translator, obj) === "XYZ_ABC")
  }
  @Test def testPlus() {
    val plus = StringOT.toUpperCase + StringOT
    assert(plus(translator, obj) === "XYZ_ABCxyz_Abc")
    assert(plus.toLowerCase.apply(translator, obj) === "xyz_abcxyz_abc")
  }
  @Test def testToUpperCamelCase() {
    assert(StringOT.toUpperCamelCase.apply(translator, obj) === "XyzAbc")
  }
  @Test def testToLowerCamelCase() {
    assert(StringOT.toLowerCamelCase.apply(translator, obj) === "xyzAbc")
  }
  @Test def testToAllCaps() {
    assert(StringOT.toAllCaps.apply(translator, obj) === "XYZ_ABC")
  }
  @Test def testToSmallCaps() {
    assert(StringOT.toSmallCaps.apply(translator, obj) === "xyz_abc")
  }
  @Test def testIndent() {
    assert(StringOT.indent.apply(translator, obj) === "  xyz_Abc")
    assert(StringOT.indent(3).apply(translator, obj) === "      xyz_Abc")
    assert(StringOT.indent("***").apply(translator, obj) === "***xyz_Abc")
  }
  @Test def testEscape() {
    assert(StringOT.escape.apply(translator, "xyz\nabc") === "xyz\\nabc")
  }
  @Test def testTrans() {
    val IntOT = new IdentityOT[Int]
    assert(IntOT.T.indent.apply(translator, 123) === "  123")
  }
  @Test def testConstant() {
    val c=StringOT.C("con")
    assert(c.str.apply(translator, obj) === "con")
  }
  @Test def testFormat() {
    val c1=StringOT.C("con")
    val c2=StringOT.C(123)
    
    assert(StringOT.format("%s-%s%d",c1.A,StringOT.A,c2.A).indent.apply(translator, obj) === "  con-xyz_Abc123")
  }
  @Test def testFormatImplicit() {
    implicit def constant[X](x:X)=StringOT.C(x)
    implicit def argument[X,Y](x:OT[X,Y])=x.A
    
    assert(StringOT.format("%s-%s%d","con",StringOT,123).indent.apply(translator, obj) == "  con-xyz_Abc123")
  }

}