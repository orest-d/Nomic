/*
This file is part of Scala Nomic Meno.

    Scala Nomic Meno is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scala Nomic Meno is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scala Nomic Meno.  If not, see <http://www.gnu.org/licenses/>.
*/

import org.scalatest.Assertions
import org.junit.Test
import eu.lateral.nomic.meno.asttranslator._
import eu.lateral.nomic.meno.parser._

class ASTTranslatorSuite extends Assertions{
  @Test def testBinary() {
    val src="""
      token      number /[0-9]+/
      keyword    plus "+"
      binary     expression on Number(plus)
      rule       main(expression)
      """
    val ast = Parser(src)
    val trans = new ASTTranslator
    trans.setProperty("package", "dummypackage")
    val translated = trans(ast) 
    assert(ast.toString.contains("Binary"))
    assert(translated.contains("class ExpressionPlus"))
    assert(translated.contains("def expressionPlus"))
  }
}