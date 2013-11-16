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

package eu.lateral.nomic.meno.defaulttrans
import eu.lateral.nomic.ObjectTranslators
import eu.lateral.nomic.ObjectTranslators.OT
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.meno.ot

object Util{
  type Translator = ObjectTranslators.TranslatorWithProperties
  implicit def toASTStringT(obj:ASTString):ASTStringT = new ASTStringT(obj)
  implicit def toASTRegexT(obj:ASTRegex):ASTRegexT = new ASTRegexT(obj)
  implicit def toIdentifierT(obj:Identifier):IdentifierT = new IdentifierT(obj)
  implicit def toNamedObjectT(obj:NamedObject):NamedObjectT = new NamedObjectT(obj)
  implicit def toClassGeneratorT(obj:ClassGenerator):ClassGeneratorT = new ClassGeneratorT(obj)
  implicit def toKeywordStatementT(obj:KeywordStatement):KeywordStatementT = new KeywordStatementT(obj)
  implicit def toKeywordParametersT(obj:KeywordParameters):KeywordParametersT = new KeywordParametersT(obj)
  implicit def toStringRegexT(obj:StringRegex):StringRegexT = new StringRegexT(obj)
  implicit def toTokenStatementT(obj:TokenStatement):TokenStatementT = new TokenStatementT(obj)
  implicit def toIgnoreStatementT(obj:IgnoreStatement):IgnoreStatementT = new IgnoreStatementT(obj)
  implicit def toRuleStatementT(obj:RuleStatement):RuleStatementT = new RuleStatementT(obj)
  implicit def toRuleElementT(obj:RuleElement):RuleElementT = new RuleElementT(obj)
  implicit def toRegisterT(obj:Register):RegisterT = new RegisterT(obj)
  implicit def toStringRuleElementT(obj:StringRuleElement):StringRuleElementT = new StringRuleElementT(obj)
  implicit def toPatternRuleElementT(obj:PatternRuleElement):PatternRuleElementT = new PatternRuleElementT(obj)
  implicit def toElementReferenceT(obj:ElementReference):ElementReferenceT = new ElementReferenceT(obj)
  implicit def toSplitByT(obj:SplitBy):SplitByT = new SplitByT(obj)
  implicit def toOneOrMoreT(obj:OneOrMore):OneOrMoreT = new OneOrMoreT(obj)
  implicit def toMoreT(obj:More):MoreT = new MoreT(obj)
  implicit def toMultiplicityT(obj:Multiplicity):MultiplicityT = new MultiplicityT(obj)
  implicit def toBinaryStatementT(obj:BinaryStatement):BinaryStatementT = new BinaryStatementT(obj)
  implicit def toGroupStatementT(obj:GroupStatement):GroupStatementT = new GroupStatementT(obj)
  implicit def toStatementT(obj:Statement):StatementT = new StatementT(obj)
  implicit def toMainT(obj:Main):MainT = new MainT(obj)
}


class ASTStringT(val obj:ASTString) extends AnyVal{
  import Util._
  def value = obj.value

  def translate(implicit translator:Translator) = value
}


class ASTRegexT(val obj:ASTRegex) extends AnyVal{
  import Util._
  def value = obj.value

  def translate(implicit translator:Translator) = value
}


class IdentifierT(val obj:Identifier) extends AnyVal{
  import Util._
  def value = obj.value

  def translate(implicit translator:Translator) = value
}


class NamedObjectT(val obj:NamedObject) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate

  def translate(implicit translator:Translator) = s"$name"
}


class ClassGeneratorT(val obj:ClassGenerator) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate

  def translate(implicit translator:Translator) = s"$name"
}


class KeywordStatementT(val obj:KeywordStatement) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate
  def keywordParameters(implicit translator:Translator) = obj.keywordParameters.map(_.translate).mkString

  def translate(implicit translator:Translator) = s"$name $keywordParameters"
}


class KeywordParametersT(val obj:KeywordParameters) extends AnyVal{
  import Util._
  def string(implicit translator:Translator) = obj.string.translate
  def o_regex(implicit translator:Translator) = obj.o_regex.map(_.translate).mkString

  def translate(implicit translator:Translator) = s"$string $o_regex"
}


class StringRegexT(val obj:StringRegex) extends AnyVal{
  import Util._

  def translate(implicit translator:Translator) = translator(obj.content)
}


class TokenStatementT(val obj:TokenStatement) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate
  def o_regex(implicit translator:Translator) = obj.o_regex.translate
  def test(implicit translator:Translator) = obj.test.list.map(_.translate).mkString

  def translate(implicit translator:Translator) = s"$name $o_regex $test"
}


class IgnoreStatementT(val obj:IgnoreStatement) extends AnyVal{
  import Util._
  def o_regex(implicit translator:Translator) = obj.o_regex.translate

  def translate(implicit translator:Translator) = s"$o_regex"
}


class RuleStatementT(val obj:RuleStatement) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate
  def sequence(implicit translator:Translator) = obj.sequence.list.map(_.translate).mkString(",")

  def translate(implicit translator:Translator) = s"$name ( $sequence )"
}


class RuleElementT(val obj:RuleElement) extends AnyVal{
  import Util._

  def translate(implicit translator:Translator) = translator(obj.content)
}


class RegisterT(val obj:Register) extends AnyVal{
  import Util._
  def identifier(implicit translator:Translator) = obj.identifier.translate

  def translate(implicit translator:Translator) = s"! $identifier"
}


class StringRuleElementT(val obj:StringRuleElement) extends AnyVal{
  import Util._
  def positionMark(implicit translator:Translator) = if (obj.positionMark) "PositionMark" else ""
  def string(implicit translator:Translator) = obj.string.translate
  def option(implicit translator:Translator) = if (obj.option) "option" else ""

  def translate(implicit translator:Translator) = s"$positionMark $string $option"
}


class PatternRuleElementT(val obj:PatternRuleElement) extends AnyVal{
  import Util._
  def positionMark(implicit translator:Translator) = if (obj.positionMark) "PositionMark" else ""
  def name(implicit translator:Translator) = obj.name.translate
  def elementReference(implicit translator:Translator) = obj.elementReference.map(_.translate).mkString
  def multiplicity(implicit translator:Translator) = obj.multiplicity.map(_.translate).mkString
  def register(implicit translator:Translator) = obj.register.map(_.translate).mkString

  def translate(implicit translator:Translator) = s"$positionMark $name $elementReference $multiplicity $register"
}


class ElementReferenceT(val obj:ElementReference) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate

  def translate(implicit translator:Translator) = s": $name"
}


class SplitByT(val obj:SplitBy) extends AnyVal{
  import Util._
  def string(implicit translator:Translator) = obj.string.translate

  def translate(implicit translator:Translator) = s"( $string )"
}


class OneOrMoreT(val obj:OneOrMore) extends AnyVal{
  import Util._
  def splitBy(implicit translator:Translator) = obj.splitBy.map(_.translate).mkString

  def translate(implicit translator:Translator) = s"+ $splitBy"
}


class MoreT(val obj:More) extends AnyVal{
  import Util._
  def splitBy(implicit translator:Translator) = obj.splitBy.map(_.translate).mkString

  def translate(implicit translator:Translator) = s"* $splitBy"
}


class MultiplicityT(val obj:Multiplicity) extends AnyVal{
  import Util._

  def translate(implicit translator:Translator) = translator(obj.content)
}


class BinaryStatementT(val obj:BinaryStatement) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate
  def operand(implicit translator:Translator) = obj.operand.translate
  def sequence(implicit translator:Translator) = obj.sequence.list.map(_.translate).mkString(",")

  def translate(implicit translator:Translator) = s"$name $operand ( $sequence )"
}


class GroupStatementT(val obj:GroupStatement) extends AnyVal{
  import Util._
  def name(implicit translator:Translator) = obj.name.translate
  def sequence(implicit translator:Translator) = obj.sequence.list.map(_.translate).mkString(",")

  def translate(implicit translator:Translator) = s"$name ( $sequence )"
}


class StatementT(val obj:Statement) extends AnyVal{
  import Util._

  def translate(implicit translator:Translator) = translator(obj.content)
}


class MainT(val obj:Main) extends AnyVal{
  import Util._
  def sequence(implicit translator:Translator) = obj.sequence.list.map(_.translate).mkString

  def translate(implicit translator:Translator) = s"$sequence"
}


class DefaultTranslator extends Util.Translator {
  import Util._
  implicit def translator:Translator = this
  override def apply(obj:Any):String = obj match{
    case x:ASTString            => x.translate
    case x:ASTRegex             => x.translate
    case x:Identifier           => x.translate
    case x:NamedObject          => x.translate
    case x:ClassGenerator       => x.translate
    case x:KeywordStatement     => x.translate
    case x:KeywordParameters    => x.translate
    case x:StringRegex          => x.translate
    case x:TokenStatement       => x.translate
    case x:IgnoreStatement      => x.translate
    case x:RuleStatement        => x.translate
    case x:RuleElement          => x.translate
    case x:Register             => x.translate
    case x:StringRuleElement    => x.translate
    case x:PatternRuleElement   => x.translate
    case x:ElementReference     => x.translate
    case x:SplitBy              => x.translate
    case x:OneOrMore            => x.translate
    case x:More                 => x.translate
    case x:Multiplicity         => x.translate
    case x:BinaryStatement      => x.translate
    case x:GroupStatement       => x.translate
    case x:Statement            => x.translate
    case x:Main                 => x.translate

    case _ => obj.toString
  }
}

                          