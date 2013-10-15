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
import eu.lateral.nomic.ObjectTranslators.{StringOT, TranslatorWithProperties, Translator}

class TranslatorSuite extends Assertions {
  def mockedTranslator = new TranslatorWithProperties {
    var msg = "Empty"

    override def error(err: AbstractError) {
      msg = err.errorMessage
    }
  }
  @Test def testError {
    val translator = mockedTranslator
    assert(translator.msg === "Empty")
    translator.error("!")
    assert(translator.msg === "ERROR: !")

  }
  @Test def testProperty {
    val translator = mockedTranslator
    assert(translator.msg === "Empty")
    assert(translator.property("x") === "")
    assert(translator.msg === "ERROR: Property 'x' not known.")
    translator.setProperty("x","y")
    assert(translator.property("x") === "y")
  }
  @Test def testPropertyOT {
    val translator = mockedTranslator
    val ot = StringOT.translatorProperty("x").toAllCaps
    assert(translator.msg === "Empty")
    assert(ot(translator,"obj") === "")
    assert(translator.msg === "ERROR: Property 'x' not known.")
    translator.setProperty("x","y")
    assert(ot(translator,"obj") === "Y")
  }
}
