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
import eu.lateral.nomic.meno.evaluatortrans._
import eu.lateral.nomic.meno.parser._
import eu.lateral.nomic.meno.ast.Main

class EvaluatorTransSuite extends Assertions{
  @Test def testBinary() {
    val src="""
      token      number /[0-9]+/
      keyword    alpha
      keyword    plus "+"
      keyword    minus "-"
      binary     expression on Number(plus, minus)
      rule       R("rule",alpha?,number*(","))
      group      statement(expression,alpha,R)
      rule       main(expression)
      """
    val ast = Parser(src)
    val trans = new EvaluatorTrans
    val translated = trans(ast)
    assert(ast.toString.contains("Binary"))
    assert(translated.contains("class ExpressionE(val obj:Expression)"))
    assert(translated.contains("class ExpressionPlusE(val obj:ExpressionPlus)"))
    assert(translated.contains("class ExpressionMinusE(val obj:ExpressionMinus)"))
    assert(translated.contains("class RE(val obj:R)"))
    assert(translated.contains("class NumberE(val obj:Number)"))
  }

  @Test def testRecursiveGroup() {
    val src="""
      token      number     /[0-9]+/
      token      identifier /[a-z]+/
      group      A(number)
      group      Rec(identifier,A)
      rule       main(rec)
      """
    val ast = Parser(src).asInstanceOf[Main]
    val rec = ast.sequence.list(3)
    val trans = new EvaluatorTrans
    val translated = trans(rec) 
    assert(ast.toString.contains("Rec"))    
    assert(rec.toString.contains("Rec"))
    assert(translated.contains("class RecE"))
  }  
}