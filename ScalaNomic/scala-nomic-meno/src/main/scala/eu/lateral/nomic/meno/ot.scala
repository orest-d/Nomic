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

package eu.lateral.nomic.meno.ot
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.ObjectTranslators


class ASTStringOT[T](first:ObjectTranslators.OT[T,ASTString]) extends ObjectTranslators.LiteralOT(first)


class ASTRegexOT[T](first:ObjectTranslators.OT[T,ASTRegex]) extends ObjectTranslators.LiteralOT(first)


class IdentifierOT[T](first:ObjectTranslators.OT[T,Identifier]) extends ObjectTranslators.LiteralOT(first)


class NamedObjectOT[T](first:ObjectTranslators.OT[T,NamedObject]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:NamedObject).name))

}

class ClassGeneratorOT[T](first:ObjectTranslators.OT[T,ClassGenerator]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:ClassGenerator).name))

}

class KeywordStatementOT[T](first:ObjectTranslators.OT[T,KeywordStatement]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:KeywordStatement).name))
  def keywordParameters    = new ObjectTranslators.ChainingOptionOT(
    this/((_:KeywordStatement).keywordParameters),
    (c:ObjectTranslators.OT[T,KeywordParameters])=>new KeywordParametersOT[T](c)
  )

}

class KeywordParametersOT[T](first:ObjectTranslators.OT[T,KeywordParameters]) extends ObjectTranslators.ASTObjectOT(first){
  def string               = new ASTStringOT(this/((_:KeywordParameters).string))
  def o_regex              = new ObjectTranslators.ChainingOptionOT(
    this/((_:KeywordParameters).o_regex),
    (c:ObjectTranslators.OT[T,StringRegex])=>new StringRegexOT[T](c)
  )

}
class StringRegexOT[T](first:ObjectTranslators.OT[T,StringRegex]) extends ObjectTranslators.ASTObjectOT(first){
  def string               = new ObjectTranslators.ChainingOptionOT(
    this/((_:StringRegex).string),
    (c:ObjectTranslators.OT[T,ASTString])=>new ASTStringOT[T](c))
  def o_regex              = new ObjectTranslators.ChainingOptionOT(
    this/((_:StringRegex).o_regex),
    (c:ObjectTranslators.OT[T,ASTRegex])=>new ASTRegexOT[T](c))

}

class TokenStatementOT[T](first:ObjectTranslators.OT[T,TokenStatement]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:TokenStatement).name))
  def o_regex              = new StringRegexOT(this/((_:TokenStatement).o_regex))
  def test                 = new ObjectTranslators.ListOT(this/((_:TokenStatement).test.list))

}

class IgnoreStatementOT[T](first:ObjectTranslators.OT[T,IgnoreStatement]) extends ObjectTranslators.ASTObjectOT(first){
  def o_regex              = new StringRegexOT(this/((_:IgnoreStatement).o_regex))

}

class RuleStatementOT[T](first:ObjectTranslators.OT[T,RuleStatement]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:RuleStatement).name))
  def sequence             = new ObjectTranslators.ListOT(this/((_:RuleStatement).sequence.list))

}
class RuleElementOT[T](first:ObjectTranslators.OT[T,RuleElement]) extends ObjectTranslators.ASTObjectOT(first){
  def stringRuleElement    = new ObjectTranslators.ChainingOptionOT(
    this/((_:RuleElement).stringRuleElement),
    (c:ObjectTranslators.OT[T,StringRuleElement])=>new StringRuleElementOT[T](c))
  def patternRuleElement   = new ObjectTranslators.ChainingOptionOT(
    this/((_:RuleElement).patternRuleElement),
    (c:ObjectTranslators.OT[T,PatternRuleElement])=>new PatternRuleElementOT[T](c))

}

class RegisterOT[T](first:ObjectTranslators.OT[T,Register]) extends ObjectTranslators.ASTObjectOT(first){
  def identifier           = new IdentifierOT(this/((_:Register).identifier))

}

class StringRuleElementOT[T](first:ObjectTranslators.OT[T,StringRuleElement]) extends ObjectTranslators.ASTObjectOT(first){
  def positionMark         = new ObjectTranslators.BooleanOT(this/((_:StringRuleElement).positionMark))
  def string               = new ASTStringOT(this/((_:StringRuleElement).string))
  def option               = new ObjectTranslators.BooleanOT(this/((_:StringRuleElement).option))

}

class PatternRuleElementOT[T](first:ObjectTranslators.OT[T,PatternRuleElement]) extends ObjectTranslators.ASTObjectOT(first){
  def positionMark         = new ObjectTranslators.BooleanOT(this/((_:PatternRuleElement).positionMark))
  def name                 = new IdentifierOT(this/((_:PatternRuleElement).name))
  def elementReference     = new ObjectTranslators.ChainingOptionOT(
    this/((_:PatternRuleElement).elementReference),
    (c:ObjectTranslators.OT[T,ElementReference])=>new ElementReferenceOT[T](c)
  )
  def multiplicity         = new ObjectTranslators.ChainingOptionOT(
    this/((_:PatternRuleElement).multiplicity),
    (c:ObjectTranslators.OT[T,Multiplicity])=>new MultiplicityOT[T](c)
  )
  def register             = new ObjectTranslators.ChainingOptionOT(
    this/((_:PatternRuleElement).register),
    (c:ObjectTranslators.OT[T,Register])=>new RegisterOT[T](c)
  )

}

class ElementReferenceOT[T](first:ObjectTranslators.OT[T,ElementReference]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:ElementReference).name))

}

class SplitByOT[T](first:ObjectTranslators.OT[T,SplitBy]) extends ObjectTranslators.ASTObjectOT(first){
  def string               = new ASTStringOT(this/((_:SplitBy).string))

}

class OneOrMoreOT[T](first:ObjectTranslators.OT[T,OneOrMore]) extends ObjectTranslators.ASTObjectOT(first){
  def splitBy              = new ObjectTranslators.ChainingOptionOT(
    this/((_:OneOrMore).splitBy),
    (c:ObjectTranslators.OT[T,SplitBy])=>new SplitByOT[T](c)
  )

}

class MoreOT[T](first:ObjectTranslators.OT[T,More]) extends ObjectTranslators.ASTObjectOT(first){
  def splitBy              = new ObjectTranslators.ChainingOptionOT(
    this/((_:More).splitBy),
    (c:ObjectTranslators.OT[T,SplitBy])=>new SplitByOT[T](c)
  )

}
class MultiplicityOT[T](first:ObjectTranslators.OT[T,Multiplicity]) extends ObjectTranslators.ASTObjectOT(first){
  def option               = new ObjectTranslators.BooleanOT(
    this/((_:Multiplicity).option))
  def oneOrMore            = new ObjectTranslators.ChainingOptionOT(
    this/((_:Multiplicity).oneOrMore),
    (c:ObjectTranslators.OT[T,OneOrMore])=>new OneOrMoreOT[T](c))
  def more                 = new ObjectTranslators.ChainingOptionOT(
    this/((_:Multiplicity).more),
    (c:ObjectTranslators.OT[T,More])=>new MoreOT[T](c))

}

class BinaryStatementOT[T](first:ObjectTranslators.OT[T,BinaryStatement]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:BinaryStatement).name))
  def operand              = new IdentifierOT(this/((_:BinaryStatement).operand))
  def sequence             = new ObjectTranslators.ListOT(this/((_:BinaryStatement).sequence.list))

}

class GroupStatementOT[T](first:ObjectTranslators.OT[T,GroupStatement]) extends ObjectTranslators.ASTObjectOT(first){
  def name                 = new IdentifierOT(this/((_:GroupStatement).name))
  def sequence             = new ObjectTranslators.ListOT(this/((_:GroupStatement).sequence.list))

}
class StatementOT[T](first:ObjectTranslators.OT[T,Statement]) extends ObjectTranslators.ASTObjectOT(first){
  def tokenStatement       = new ObjectTranslators.ChainingOptionOT(
    this/((_:Statement).tokenStatement),
    (c:ObjectTranslators.OT[T,TokenStatement])=>new TokenStatementOT[T](c))
  def keywordStatement     = new ObjectTranslators.ChainingOptionOT(
    this/((_:Statement).keywordStatement),
    (c:ObjectTranslators.OT[T,KeywordStatement])=>new KeywordStatementOT[T](c))
  def ruleStatement        = new ObjectTranslators.ChainingOptionOT(
    this/((_:Statement).ruleStatement),
    (c:ObjectTranslators.OT[T,RuleStatement])=>new RuleStatementOT[T](c))
  def groupStatement       = new ObjectTranslators.ChainingOptionOT(
    this/((_:Statement).groupStatement),
    (c:ObjectTranslators.OT[T,GroupStatement])=>new GroupStatementOT[T](c))
  def ignoreStatement      = new ObjectTranslators.ChainingOptionOT(
    this/((_:Statement).ignoreStatement),
    (c:ObjectTranslators.OT[T,IgnoreStatement])=>new IgnoreStatementOT[T](c))
  def binaryStatement      = new ObjectTranslators.ChainingOptionOT(
    this/((_:Statement).binaryStatement),
    (c:ObjectTranslators.OT[T,BinaryStatement])=>new BinaryStatementOT[T](c))

}

class MainOT[T](first:ObjectTranslators.OT[T,Main]) extends ObjectTranslators.ASTObjectOT(first){
  def sequence             = new ObjectTranslators.ListOT(this/((_:Main).sequence.list))

}

