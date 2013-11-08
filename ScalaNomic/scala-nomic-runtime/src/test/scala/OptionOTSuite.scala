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

import eu.lateral.nomic.errors.AbstractError
import org.scalatest.Assertions
import org.junit.Test
import eu.lateral.nomic.ASTObjects._
import eu.lateral.nomic.ObjectTranslators._
import util.parsing.input.NoPosition

class OptionOTSuite extends Assertions{
  val text="xyz_Abc"
  val obj = Some(text)
  val translator = new Translator()
  val ot = new IdentityOptionOT[String]

  @Test def testIdentity() {
    assert(ot(translator, obj) === obj)
  }
  @Test def testGetOrElse() {
    assert(ot.getOrElse("x").apply(translator, obj) === text)
    assert(ot.getOrElse("x").apply(translator, None) === "x")
  }
  @Test def testGetOrElseOT() {
    assert(ot.getOrElse(ot.C("x")).apply(translator, obj) === text)
    assert(ot.getOrElse(ot.C("x")).apply(translator, None) === "x")
  }
  @Test def testTranslateOrElse() {
    assert(ot.translateOrElse("x").toUpperCase(translator, obj) === "XYZ_ABC")
    assert(ot.translateOrElse("x").toUpperCase(translator, None) === "X")
  }
  @Test def testTranslateOrElseOT() {
    assert(ot.translateOrElse(ot.C("x")).toUpperCase(translator, obj) === "XYZ_ABC")
    assert(ot.translateOrElse(ot.C("x")).toUpperCase(translator, None) === "X")
  }
  @Test def testGet() {
    assert(ot.get(translator, obj) === text)
  }
  @Test def testIsDefined() {
    assert(ot.isDefined(translator, obj) === true)
    assert(ot.isDefined(translator, None) === false)
  }
  @Test def testOptionCompose() {
    assert((ot?StringOT.toUpperCase).getOrElse("x").apply(translator, obj) === "XYZ_ABC")
    assert((ot?StringOT.toUpperCase).getOrElse("x").apply(translator, None) === "x")
  }
  @Test def testStr() {
    assert((ot.str("x").toUpperCase).apply(translator, obj) === "XYZ_ABC")
    assert((ot.str("x").toUpperCase).apply(translator, None) === "X")
  }
  @Test def testT() {
    assert((ot.T("x").toUpperCase).apply(translator, obj) === "XYZ_ABC")
    assert((ot.T("x").toUpperCase).apply(translator, None) === "X")
  }
  @Test def testIfDefinedOrElse() {
    assert(ot.ifDefinedOrElse(ot.C(1),ot.C(2)).apply(translator, obj) === 1)
    assert(ot.ifDefinedOrElse(ot.C(1),ot.C(2)).apply(translator, None) === 2)
  }
  @Test def testStringIfDefinedOrElse() {
    assert(ot.ifDefinedOrElse(ot.C("a"),ot.C("b")).toUpperCase.apply(translator, obj) === "A")
    assert(ot.ifDefinedOrElse(ot.C("a"),ot.C("b")).toUpperCase.apply(translator, None) === "B")
  }
  @Test def testChaining() {
    val ch = new ChainingOptionOT[Option[String],String,StringOT[Option[String]]](ot,new StringOT[Option[String]](_))
    assert(ch.get.toUpperCase(translator,obj) === "XYZ_ABC")
  }
  @Test def testGetOrFail() {
    val mockedTranslator = new Translator {
      var msg = "Empty"

      override def error(err: AbstractError) {
        msg = err.errorMessage
      }
    }

    val ch = new ChainingOptionOT[Option[String],String,StringOT[Option[String]]](ot,new StringOT[Option[String]](_))
    assert(ch.getOrFail(ch.C("message")).toUpperCase(mockedTranslator,obj) === "XYZ_ABC")
    assert(mockedTranslator.msg === "Empty")
    try {
      ch.getOrFail(ch.C("message")).toUpperCase(mockedTranslator,None)
    }
    catch{
      case _:Throwable => ()
    }
    assert(mockedTranslator.msg === "ERROR: message")

  }

  @Test def testGetOrFailPositioned() {
    val mockedTranslator = new Translator {
      var msg = "Empty"

      override def error(err: AbstractError) {
        msg = err.errorMessage
      }
    }

    val ch = new ChainingOptionOT[Option[String],String,StringOT[Option[String]]](ot,new StringOT[Option[String]](_))
    assert(ch.getOrFail(ch.C("message"),ch.C(NoPosition)).toUpperCase(mockedTranslator,obj) === "XYZ_ABC")
    assert(mockedTranslator.msg === "Empty")
    try {
      ch.getOrFail(ch.C("message"),ch.C(NoPosition)).toUpperCase(mockedTranslator,None)
    }
    catch{
      case _:Throwable => ()
    }
    assert(mockedTranslator.msg === "ERROR: message <undefined position>")

  }

}