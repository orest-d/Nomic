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

package eu.lateral.nomic.meno.asttrans
import eu.lateral.nomic.ObjectTranslators
import eu.lateral.nomic.ObjectTranslators.OT
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.ASTObjects.ASTObject
import eu.lateral.nomic.errors.PositionError
import eu.lateral.nomic.TextUtils
import eu.lateral.nomic.TextUtils._
import eu.lateral.nomic.meno.CommonUtils
import eu.lateral.nomic.meno.MenoTranslatorBase

object Util extends CommonUtils{
  type Translator = ASTTranslator
  implicit def toASTStringT(obj: ASTString): ASTStringT = new ASTStringT(obj)
  implicit def toASTRegexT(obj: ASTRegex): ASTRegexT = new ASTRegexT(obj)
  implicit def toIdentifierT(obj: Identifier): IdentifierT = new IdentifierT(obj)
  implicit def toNamedObjectT(obj: NamedObject): NamedObjectT = new NamedObjectT(obj)
  implicit def toClassGeneratorT(obj: ClassGenerator): ClassGeneratorT = new ClassGeneratorT(obj)
  implicit def toKeywordStatementT(obj: KeywordStatement): KeywordStatementT = new KeywordStatementT(obj)
  implicit def toKeywordParametersT(obj: KeywordParameters): KeywordParametersT = new KeywordParametersT(obj)
  implicit def toStringRegexT(obj: StringRegex): StringRegexT = new StringRegexT(obj)
  implicit def toTokenStatementT(obj: TokenStatement): TokenStatementT = new TokenStatementT(obj)
  implicit def toIgnoreStatementT(obj: IgnoreStatement): IgnoreStatementT = new IgnoreStatementT(obj)
  implicit def toRuleStatementT(obj: RuleStatement): RuleStatementT = new RuleStatementT(obj)
  implicit def toRuleElementT(obj: RuleElement): RuleElementT = new RuleElementT(obj)
  implicit def toRegisterT(obj: Register): RegisterT = new RegisterT(obj)
  implicit def toStringRuleElementT(obj: StringRuleElement): StringRuleElementT = new StringRuleElementT(obj)
  implicit def toPatternRuleElementT(obj: PatternRuleElement): PatternRuleElementT = new PatternRuleElementT(obj)
  implicit def toElementReferenceT(obj: ElementReference): ElementReferenceT = new ElementReferenceT(obj)
  implicit def toSplitByT(obj: SplitBy): SplitByT = new SplitByT(obj)
  implicit def toOneOrMoreT(obj: OneOrMore): OneOrMoreT = new OneOrMoreT(obj)
  implicit def toMoreT(obj: More): MoreT = new MoreT(obj)
  implicit def toMultiplicityT(obj: Multiplicity): MultiplicityT = new MultiplicityT(obj)
  implicit def toBinaryStatementT(obj: BinaryStatement): BinaryStatementT = new BinaryStatementT(obj)
  implicit def toGroupStatementT(obj: GroupStatement): GroupStatementT = new GroupStatementT(obj)
  implicit def toStatementT(obj: Statement): StatementT = new StatementT(obj)
  implicit def toMainT(obj: Main): MainT = new MainT(obj)
}

class ASTStringT(val obj: ASTString) extends AnyVal {
  import Util._
  def value = obj.value

  def translate(implicit translator: Translator) = value
}

class ASTRegexT(val obj: ASTRegex) extends AnyVal {
  import Util._
  def value = obj.value

  def translate(implicit translator: Translator) = value
}

class IdentifierT(val obj: Identifier) extends AnyVal {
  import Util._
  def value = obj.value

  def ref(implicit translator: Translator) = translator.ref(obj)
  def refSafe(implicit translator: Translator) = translator.refSafe(obj)
  def keyword(implicit translator: Translator) = translator.keyword(obj)
  def token(implicit translator: Translator) = translator.token(obj)
  def rule(implicit translator: Translator) = translator.rule(obj)
  def group(implicit translator: Translator) = translator.group(obj)
  def binary(implicit translator: Translator) = translator.binary(obj)
  def refname = obj.value.refname
  def astname = obj.value.astname

  def regularGroupMethod(implicit translator: Translator) = s"""|  def $refname:Option[$astname] = content match {
    |    case x:$astname => Some(x)
    |    case _ => None
    |  }
    |""".stripMargin
  def booleanGroupMethod(implicit translator: Translator) = s"""|  def $refname:Boolean = content match {
    |    case $astname => true
    |    case _ => false
    |  }
    |""".stripMargin
  def groupMethod(implicit translator: Translator) = {
    if (refSafe.keywordStatement.isDefined) {
      booleanGroupMethod
    } else {
      regularGroupMethod
    }
  }

  def binaryName = {
    obj.ancestor[BinaryStatement] match {
      case Some(bs) => bs.name.value.astname
      case None => throw PositionError(s"Identifier $value not in binary", obj.pos)
    }
  }

  def binaryOperatorObjectName = binaryName + astname
  def binaryMethodName = binaryOperatorObjectName.refname
  def binaryASTClass = s"""|case class $binaryOperatorObjectName(override val left:$binaryName, override val right:$binaryName) extends ASTObjects.ABinary(left,right){
    |  override def toString = s"$binaryOperatorObjectName($$left, $$right)" 
    |}
    |""".stripMargin
  def binaryMethods = s"""|  def $binaryMethodName:Option[$binaryOperatorObjectName] = content match {
    |    case x:$binaryOperatorObjectName => Some(x)
    |    case _ => None
    |  }
    |  def $refname = $binaryMethodName
    |""".stripMargin
  def translate(implicit translator: Translator) = value

  def referencedGroupMembers(implicit translator: Translator): List[Statement] = {
    val statement = refSafe
    if (translator.expandGroup) {
      if (statement.groupStatement.isDefined) {
        statement.groupStatement.get.sequence.list.flatMap(_.referencedGroupMembers)
      } else {
        List(statement)
      }
    } else {
      List(statement)
    }
  }
}

class NamedObjectT(val obj: NamedObject) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate

  def translate(implicit translator: Translator) = s"$name"
}

class ClassGeneratorT(val obj: ClassGenerator) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate

  def translate(implicit translator: Translator) = s"$name"
}

class KeywordStatementT(val obj: KeywordStatement) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate
  def astname = obj.name.value.astname
  def keywordParameters(implicit translator: Translator) = obj.keywordParameters.map(_.translate).mkString

  def translate(implicit translator: Translator) = s"case object $astname extends ASTObjects.ASTObject\n\n"
}

class KeywordParametersT(val obj: KeywordParameters) extends AnyVal {
  import Util._
  def string(implicit translator: Translator) = obj.string.translate
  def o_regex(implicit translator: Translator) = obj.o_regex.map(_.translate).mkString

  def translate(implicit translator: Translator) = s"$string $o_regex"
}

class StringRegexT(val obj: StringRegex) extends AnyVal {
  import Util._

  def translate(implicit translator: Translator) = translator(obj.content)
}

class TokenStatementT(val obj: TokenStatement) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate
  def o_regex(implicit translator: Translator) = obj.o_regex.translate
  def test(implicit translator: Translator) = obj.test.list.map(_.translate).mkString
  def astname = obj.name.value.astname

  def translate(implicit translator: Translator) = s"case class $astname(value :String) extends ASTObjects.Literal(value)\n\n"
}

class IgnoreStatementT(val obj: IgnoreStatement) extends AnyVal {
  import Util._
  def o_regex(implicit translator: Translator) = obj.o_regex.translate

  def translate(implicit translator: Translator) = ""
}

class RuleStatementT(val obj: RuleStatement) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.translate).join(" ")
  def astname(implicit translator: Translator) = name.astname
  def arguments(implicit translator: Translator) = obj.sequence.list.map(_.argument).join(", ")
  def translate(implicit translator: Translator) = s"""|case class $astname($arguments) extends ASTObjects.ASTObject{
    |$sequence}
    |""".stripMargin
}

class RuleElementT(val obj: RuleElement) extends AnyVal {
  import Util._
  def translate(implicit translator: Translator) = translator(obj.content)
  def argument(implicit translator: Translator) = obj.content match {
    case x: PatternRuleElement => x.argument
    case _ => ""
  }
}

class RegisterT(val obj: Register) extends AnyVal {
  import Util._
  def identifier(implicit translator: Translator) = obj.identifier.translate

  def translate(implicit translator: Translator) = s"! $identifier"
}

class StringRuleElementT(val obj: StringRuleElement) extends AnyVal {
  import Util._
  def positionMark(implicit translator: Translator) = if (obj.positionMark) "PositionMark" else ""
  def string = obj.string.value
  def stringContent = string.substring(1, string.length - 1)
  def option(implicit translator: Translator) = if (obj.option) "option" else ""

  def translate(implicit translator: Translator) = ""
}

class PatternRuleElementT(val obj: PatternRuleElement) extends AnyVal {
  import Util._
  def positionMark(implicit translator: Translator) = if (obj.positionMark) "PositionMark" else ""
  def name(implicit translator: Translator) = obj.name.translate
  def keyword(implicit translator: Translator) = obj.name.translate
  def quotedKeyword(implicit translator: Translator) = "\"" + keyword + "\""
  def elementReference(implicit translator: Translator) = obj.elementReference.map(_.translate).mkString
  def multiplicity(implicit translator: Translator) = obj.multiplicity.map(_.translate).mkString
  def register(implicit translator: Translator) = obj.register.map(_.translate).mkString
  def ref(implicit translator: Translator) = {
    if (obj.elementReference.isDefined) {
      obj.elementReference.get.name.refSafe
    } else {
      obj.name.refSafe
    }
  }
  def refname = obj.name.value.refname
  def reftype = if (obj.elementReference.isDefined) {
    obj.elementReference.get.name.value.astname
  } else {
    obj.name.value.astname
  }
  def isEmpty(implicit translator: Translator) = ((!obj.multiplicity.isDefined) && (ref.keywordStatement.isDefined))
  def isBoolean(implicit translator: Translator) = ((obj.multiplicity.isDefined) && (ref.keywordStatement.isDefined) && (obj.multiplicity.get.option))
  def isOption(implicit translator: Translator) = ((obj.multiplicity.isDefined) && (!ref.keywordStatement.isDefined) && (obj.multiplicity.get.option))
  def isList(implicit translator: Translator) = (
    (obj.multiplicity.isDefined) &&
    (!ref.keywordStatement.isDefined) &&
    (obj.multiplicity.get.more.isDefined || obj.multiplicity.get.oneOrMore.isDefined))

  def fullType(implicit translator: Translator) = {
    if (isBoolean) {
      "Boolean"
    } else if (isList){
      s"ASTObjects.AList[$reftype]"
    } else if (isOption){
      s"Option[$reftype]"      
    }
    else{
      reftype
    }
  }

  def parent(implicit translator: Translator) = {
    if (isBoolean || isEmpty) {
      ""
    } else if (isList){
      s"  $refname.parent = Some(this)\n"
    } else if (isOption){
      s"  $refname.foreach(_.parent=Some(this))\n"      
    }
    else{
      s"  $refname.parent = Some(this)\n"      
    }
  }

  def argument(implicit translator: Translator) = if (isEmpty) "" else s"$refname:$fullType"
  def translate(implicit translator: Translator) = parent
}

class ElementReferenceT(val obj: ElementReference) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate

  def translate(implicit translator: Translator) = s": $name"
}

class SplitByT(val obj: SplitBy) extends AnyVal {
  import Util._
  def string(implicit translator: Translator) = obj.string.translate

  def translate(implicit translator: Translator) = string
}

class OneOrMoreT(val obj: OneOrMore) extends AnyVal {
  import Util._
  def splitBy(implicit translator: Translator) = obj.splitBy.map(_.translate).mkString

  def translate(implicit translator: Translator) = s"""($splitBy)"""
}

class MoreT(val obj: More) extends AnyVal {
  import Util._
  def splitBy(implicit translator: Translator) = obj.splitBy.map("(" + _.translate + ")").mkString

  def translate(implicit translator: Translator) = splitBy
}

class MultiplicityT(val obj: Multiplicity) extends AnyVal {
  import Util._

  def translate(implicit translator: Translator) = if (obj.option) "" else translator(obj.content)
}

class BinaryStatementT(val obj: BinaryStatement) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate
  def operand(implicit translator: Translator) = obj.operand.translate
  def operandRefname = obj.operand.value.refname
  def operandASTname = obj.operand.value.astname
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.translate).join(",")
  def astname(implicit translator: Translator) = name.astname
  def cname(implicit translator: Translator) = name.cname
  def methods(implicit translator: Translator) = obj.sequence.list.map(_.binaryMethods).mkString
  def derivedClasses(implicit translator: Translator) = obj.sequence.list.map(_.binaryASTClass).join("\n")
  def baseMethod = s"""|  def $operandRefname:Option[$operandASTname] = content match {
    |    case x:$operandASTname => Some(x)
    |    case _ => None
    |  }""".stripMargin
  def classes(implicit translator: Translator) = s"""|case class $astname(content:ASTObjects.ASTObject) extends ASTObjects.AGroup(content){
    |$baseMethod
    |$methods}
    |
    |$derivedClasses
    |""".stripMargin

  def translate(implicit translator: Translator) = classes
}

class GroupStatementT(val obj: GroupStatement) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.groupMethod).mkString
  def members(implicit translator: Translator) = {
    val m = for (r <- referencedGroupMembers; id <- r.identifier) yield id.groupMethod
    m.mkString
  }
  def astname(implicit translator: Translator) = name.astname
  def cname(implicit translator: Translator) = name.cname
  def classes(implicit translator: Translator) = s"""|case class $astname(content:ASTObjects.ASTObject) extends ASTObjects.AGroup(content){
    |$members
    |  override def toString = s"$astname($$content)"
    |}
    |
    |""".stripMargin

  def translate(implicit translator: Translator) = classes

  def referencedGroupMembers(implicit translator: Translator) = {
    val m = for (
      name <- obj.sequence.list;
      member <- name.referencedGroupMembers
    ) yield member
    scala.collection.mutable.LinkedHashSet(m: _*).toList
  }
}

class StatementT(val obj: Statement) extends AnyVal {
  import Util._

  def identifier(implicit translator: Translator) = obj.content match {
    case x: KeywordStatement => Some(x.name)
    case x: TokenStatement => Some(x.name)
    case x: RuleStatement => Some(x.name)
    case x: GroupStatement => Some(x.name)
    case x: BinaryStatement => Some(x.name)
    case _ => None
  }
  def translate(implicit translator: Translator) = translator(obj.content)
}

class MainT(val obj: Main) extends AnyVal {
  import Util._
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.translate).mkString
  def pkg(implicit translator: Translator) = translator.pkg

  def translate(implicit translator: Translator) = s"""package $pkg.ast
    |import eu.lateral.nomic.ASTObjects
    |
    |$sequence
    |""".stripMargin
}

class ASTTranslator extends MenoTranslatorBase {
  import Util._
  implicit def translator: Translator = this

  override def apply(obj: Any): String = obj match {
    case x: ASTString => x.translate
    case x: ASTRegex => x.translate
    case x: Identifier => x.translate
    case x: NamedObject => x.translate
    case x: ClassGenerator => x.translate
    case x: KeywordStatement => x.translate
    case x: KeywordParameters => x.translate
    case x: StringRegex => x.translate
    case x: TokenStatement => x.translate
    case x: IgnoreStatement => x.translate
    case x: RuleStatement => x.translate
    case x: RuleElement => x.translate
    case x: Register => x.translate
    case x: StringRuleElement => x.translate
    case x: PatternRuleElement => x.translate
    case x: ElementReference => x.translate
    case x: SplitBy => x.translate
    case x: OneOrMore => x.translate
    case x: More => x.translate
    case x: Multiplicity => x.translate
    case x: BinaryStatement => x.translate
    case x: GroupStatement => x.translate
    case x: Statement => x.translate
    case x: Main => x.translate

    case _ => obj.toString
  }
}

