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

package eu.lateral.nomic.meno.ast

import eu.lateral.nomic.ASTObjects

case object Keyword extends ASTObjects.ASTObject

case object Token extends ASTObjects.ASTObject

case object Rule extends ASTObjects.ASTObject

case object Group extends ASTObjects.ASTObject

case object Ignore extends ASTObjects.ASTObject

case object ASTOption extends ASTObjects.ASTObject

case object PositionMark extends ASTObjects.ASTObject

case object Binary extends ASTObjects.ASTObject

case class ASTString(override val value :String) extends ASTObjects.Literal(value)

case class ASTRegex(override val value :String) extends ASTObjects.Literal(value)

case class Identifier(override val value :String) extends ASTObjects.Literal(value)

case class NamedObject(name:Identifier) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
}

case class ClassGenerator(name:Identifier) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
}

case class KeywordStatement(name:Identifier, keywordParameters:Option[KeywordParameters]) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
  keywordParameters.foreach(_.parent=Some(this))
}

case class KeywordParameters(string:ASTString, o_regex:Option[StringRegex]) extends ASTObjects.ASTObject{
  string.parent        = Some(this)
  o_regex.foreach(_.parent=Some(this))
}

case class StringRegex(override val content:ASTObjects.ASTObject) extends ASTObjects.AGroup(content){
  def string:Option[ASTString] = content match {
    case x:ASTString => Some(x)
    case _ => None
  }
  def o_regex:Option[ASTRegex] = content match {
    case x:ASTRegex => Some(x)
    case _ => None
  }

}
case class TokenStatement(name:Identifier, o_regex:StringRegex, test:ASTObjects.AList[ASTString]) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
  o_regex.parent       = Some(this)
  test.parent          = Some(this)
}

case class IgnoreStatement(o_regex:StringRegex) extends ASTObjects.ASTObject{
  o_regex.parent       = Some(this)
}

case class RuleStatement(name:Identifier, sequence:ASTObjects.AList[RuleElement]) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
  sequence.parent      = Some(this)
}

case class RuleElement(override val content:ASTObjects.ASTObject) extends ASTObjects.AGroup(content){
  def stringRuleElement:Option[StringRuleElement] = content match {
    case x:StringRuleElement => Some(x)
    case _ => None
  }
  def patternRuleElement:Option[PatternRuleElement] = content match {
    case x:PatternRuleElement => Some(x)
    case _ => None
  }

}
case class Register(identifier:Identifier) extends ASTObjects.ASTObject{
  identifier.parent    = Some(this)
}

case class StringRuleElement(positionMark : Boolean, string:ASTString, option : Boolean) extends ASTObjects.ASTObject{
  string.parent        = Some(this)
}

case class PatternRuleElement(positionMark : Boolean, name:Identifier, elementReference:Option[ElementReference], multiplicity:Option[Multiplicity], register:Option[Register]) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
  elementReference.foreach(_.parent=Some(this))
  multiplicity.foreach(_.parent=Some(this))
  register.foreach(_.parent=Some(this))
}

case class ElementReference(name:Identifier) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
}

case class SplitBy(string:ASTString) extends ASTObjects.ASTObject{
  string.parent        = Some(this)
}

case class OneOrMore(splitBy:Option[SplitBy]) extends ASTObjects.ASTObject{
  splitBy.foreach(_.parent=Some(this))
}

case class More(splitBy:Option[SplitBy]) extends ASTObjects.ASTObject{
  splitBy.foreach(_.parent=Some(this))
}

case class Multiplicity(override val content:ASTObjects.ASTObject) extends ASTObjects.AGroup(content){
  def option:Boolean = content match {
    case ASTOption => true
    case _ => false
  }
  def oneOrMore:Option[OneOrMore] = content match {
    case x:OneOrMore => Some(x)
    case _ => None
  }
  def more:Option[More] = content match {
    case x:More => Some(x)
    case _ => None
  }

}
case class BinaryStatement(name:Identifier, operand:Identifier, sequence:ASTObjects.AList[Identifier]) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
  operand.parent       = Some(this)
  sequence.parent      = Some(this)
}

case class GroupStatement(name:Identifier, sequence:ASTObjects.AList[Identifier]) extends ASTObjects.ASTObject{
  name.parent          = Some(this)
  sequence.parent      = Some(this)
}

case class Statement(override val content:ASTObjects.ASTObject) extends ASTObjects.AGroup(content){
  def tokenStatement:Option[TokenStatement] = content match {
    case x:TokenStatement => Some(x)
    case _ => None
  }
  def keywordStatement:Option[KeywordStatement] = content match {
    case x:KeywordStatement => Some(x)
    case _ => None
  }
  def ruleStatement:Option[RuleStatement] = content match {
    case x:RuleStatement => Some(x)
    case _ => None
  }
  def groupStatement:Option[GroupStatement] = content match {
    case x:GroupStatement => Some(x)
    case _ => None
  }
  def ignoreStatement:Option[IgnoreStatement] = content match {
    case x:IgnoreStatement => Some(x)
    case _ => None
  }
  def binaryStatement:Option[BinaryStatement] = content match {
    case x:BinaryStatement => Some(x)
    case _ => None
  }

}
case class Main(sequence:ASTObjects.AList[Statement]) extends ASTObjects.ASTObject{
  sequence.parent      = Some(this)
}


