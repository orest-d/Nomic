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

package eu.lateral.nomic.meno.todefaulttrans
import eu.lateral.nomic.ObjectTranslators
import eu.lateral.nomic.ObjectTranslators.OT
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.ASTObjects.ASTObject
import eu.lateral.nomic.errors.PositionError
import eu.lateral.nomic.TextUtils._
import eu.lateral.nomic.meno.CommonUtils
import eu.lateral.nomic.meno.MenoTranslatorBase

object Util extends CommonUtils{
  type Translator = ToDefaultTrans
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

  def binaryName = {
    obj.ancestor[BinaryStatement] match {
      case Some(bs) => bs.name.value.astname
      case None => throw PositionError(s"Identifier $value not in binary", obj.pos)
    }
  }

  def binaryOperatorObjectName = binaryName + obj.value.astname
  def binaryOperatorObjectNameT = binaryOperatorObjectName.cname
  
  def binaryTranslationClass = s"""|class $binaryOperatorObjectNameT(val obj:$binaryOperatorObjectName) extends AnyVal{
    |  import Util._
    |  def left(implicit translator:Translator)  = obj.left.translate
    |  def right(implicit translator:Translator) = obj.right.translate
    |
    |  def translate(implicit translator:Translator) = s"$$left $value $$right"
    |}
    |""".stripMargin
  def binaryCase = "    case x:%-20s => x.translate\n".format(binaryOperatorObjectName)
  def binaryImplicit = s"  implicit def to$binaryOperatorObjectNameT(obj:$binaryOperatorObjectName):$binaryOperatorObjectNameT = new $binaryOperatorObjectNameT(obj)\n"
  def translate(implicit translator: Translator) = value
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
  def keywordParameters(implicit translator: Translator) = obj.keywordParameters.map(_.translate).mkString

  def translate(implicit translator: Translator) = ""
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
  def astname(implicit translator: Translator) = name.astname
  def cname(implicit translator: Translator) = name.cname
  def implicits(implicit translator: Translator) = s"  implicit def to$cname(obj:$astname):$cname = new $cname(obj)\n"
  def classes(implicit translator: Translator) = s"""|class $cname(val obj:$astname) extends AnyVal{
    |  import Util._
    |  def value = obj.value
    |
    |  def translate(implicit translator:Translator) = value
    |}
    |""".stripMargin

  def cases(implicit translator: Translator) = "    case x:%-20s => x.translate\n".format(astname)

  def translate(implicit translator: Translator) = classes
}

class IgnoreStatementT(val obj: IgnoreStatement) extends AnyVal {
  import Util._
  def o_regex(implicit translator: Translator) = obj.o_regex.translate

  def translate(implicit translator: Translator) = ""
}

class RuleStatementT(val obj: RuleStatement) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.translate).mkString(" ")
  def astname(implicit translator: Translator) = name.astname
  def cname(implicit translator: Translator) = name.cname
  def implicits(implicit translator: Translator) = s"  implicit def to$cname(obj:$astname):$cname = new $cname(obj)\n"
  def methods(implicit translator: Translator) = obj.sequence.list.map(_.methods).mkString
  def classes(implicit translator: Translator) = s"""|class $cname(val obj:$astname) extends AnyVal{
    |  import Util._
    |$methods
    |  def translate(implicit translator:Translator) = s"$sequence"
    |}
    |""".stripMargin

  def cases(implicit translator: Translator) = "    case x:%-20s => x.translate\n".format(astname)
  def translate(implicit translator: Translator) = classes
}

class RuleElementT(val obj: RuleElement) extends AnyVal {
  import Util._
  def methods(implicit translator: Translator) = obj.patternRuleElement.map(_.method).mkString
  def translate(implicit translator: Translator) = translator(obj.content)
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

  def translate(implicit translator: Translator) = stringContent
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

  def methodDefinition(implicit translator: Translator) = s"  def $refname(implicit translator:Translator) = $body\n"
  def method(implicit translator: Translator) = {
    if (obj.multiplicity.isDefined) {
      methodDefinition
    } else {
      if (ref.keywordStatement.isDefined) {
        ""
      } else {
        methodDefinition
      }
    }
  }
  def body(implicit translator: Translator) = {
    if (obj.multiplicity.isDefined) {
      if (obj.multiplicity.get.option) {
        if (ref.keywordStatement.isDefined) {
          bodyBoolean
        } else {
          bodyOption
        }
      } else {
        bodyList
      }
    } else {
      bodySimple
    }
  }
  def bodySimple(implicit translator: Translator) = s"obj.$refname.translate"
  def bodyBoolean(implicit translator: Translator) = s"""if (obj.$refname) $quotedKeyword else """""
  def bodyOption(implicit translator: Translator) = s"obj.$refname.map(_.translate).mkString"
  def bodyList(implicit translator: Translator) = s"obj.$refname.list.map(_.translate).mkString$multiplicity"
  def expansionSimple(implicit translator: Translator) = "$" + refname
  def expansion(implicit translator: Translator) = {
    if (obj.multiplicity.isDefined) {
      expansionSimple
    } else {
      if (ref.keywordStatement.isDefined) {
        keyword
      } else {
        expansionSimple
      }
    }
  }

  def translate(implicit translator: Translator) = expansion
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
  def splitBy(implicit translator: Translator) = obj.splitBy.map("("+_.translate+")").mkString

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
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.translate).mkString(",")
  def astname(implicit translator: Translator) = name.astname
  def cname(implicit translator: Translator) = name.cname
  def derivedClasses(implicit translator: Translator) = obj.sequence.list.map(_.binaryTranslationClass).mkString("\n")
  def derivedImplicits(implicit translator: Translator) = obj.sequence.list.map(_.binaryImplicit).mkString
  def derivedCases(implicit translator: Translator) = obj.sequence.list.map(_.binaryCase).mkString
  def implicits(implicit translator: Translator) = s"  implicit def to$cname(obj:$astname):$cname = new $cname(obj)\n$derivedImplicits"
  def classes(implicit translator: Translator) = s"""$derivedClasses
    |class $cname(val obj:$astname) extends AnyVal{
    |  import Util._
    |
    |  def translate(implicit translator:Translator) = translator(obj.content)
    |}
    |""".stripMargin

  def cases(implicit translator: Translator) = "    case x:%-20s => x.translate\n%s".format(astname,derivedCases)

  def translate(implicit translator: Translator) = classes
}

class GroupStatementT(val obj: GroupStatement) extends AnyVal {
  import Util._
  def name(implicit translator: Translator) = obj.name.translate
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.translate).mkString(",")
  def astname(implicit translator: Translator) = name.astname
  def cname(implicit translator: Translator) = name.cname
  def implicits(implicit translator: Translator) = s"  implicit def to$cname(obj:$astname):$cname = new $cname(obj)\n"
  def classes(implicit translator: Translator) = s"""|class $cname(val obj:$astname) extends AnyVal{
    |  import Util._
    |
    |  def translate(implicit translator:Translator) = translator(obj.content)
    |}
    |""".stripMargin

  def cases(implicit translator: Translator) = "    case x:%-20s => x.translate\n".format(astname)

  def translate(implicit translator: Translator) = classes
}

class StatementT(val obj: Statement) extends AnyVal {
  import Util._

  def implicits(implicit translator: Translator) = obj.content match {
    case x: RuleStatement => x.implicits
    case x: GroupStatement => x.implicits
    case x: BinaryStatement => x.implicits
    case x: TokenStatement => x.implicits
    case _ => ""
  }
  def classes(implicit translator: Translator) = obj.content match {
    case x: RuleStatement => x.classes
    case x: GroupStatement => x.classes
    case x: BinaryStatement => x.classes
    case x: TokenStatement => x.classes
    case _ => ""
  }
  def cases(implicit translator: Translator) = obj.content match {
    case x: RuleStatement => x.cases
    case x: GroupStatement => x.cases
    case x: BinaryStatement => x.cases
    case x: TokenStatement => x.cases
    case _ => ""
  }
  def translate(implicit translator: Translator) = translator(obj.content)
}

class MainT(val obj: Main) extends AnyVal {
  import Util._
  def sequence(implicit translator: Translator) = obj.sequence.list.map(_.translate).mkString
  def pkg(implicit translator: Translator) = translator.pkg
  def implicits(implicit translator: Translator) = obj.sequence.list.map(_.implicits).mkString
  def classes(implicit translator: Translator) = obj.sequence.list.map(_.classes).mkString("\n")
  def cases(implicit translator: Translator) = obj.sequence.list.map(_.cases).mkString

  def translate(implicit translator: Translator) = s"""package $pkg.defaulttrans
    |import ${translator.fullASTPackageName}._
    |
    |object Util{
    |  type Translator = DefaultTranslator
    |$implicits}
    |
    |$classes
    |class DefaultTranslator{
    |  import Util._
    |  implicit def translator:Translator = this
    |  def apply(obj:Any):String = obj match{
    |$cases
    |    case _ => obj.toString
    |  }
    |}
    |""".stripMargin
}

class ToDefaultTrans extends MenoTranslatorBase {
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

                          