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

package eu.lateral.nomic.meno.toprologgenerator

import eu.lateral.nomic.meno.todefaulttranslator
import eu.lateral.nomic.meno.{Util, ot}
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.ObjectTranslators
import eu.lateral.nomic.ASTObjects.ASTObject

object ASTStringT extends ASTStringT(new ObjectTranslators.IdentityOT[ASTString])

class ASTStringT[T](first: ObjectTranslators.OT[T, ASTString]) extends ot.ASTStringOT(first) {

  def translate = new ObjectTranslators.StringOT(value/{x => x.substring(1,x.length-1)})
}


object ASTRegexT extends ASTRegexT(new ObjectTranslators.IdentityOT[ASTRegex])

class ASTRegexT[T](first: ObjectTranslators.OT[T, ASTRegex]) extends ot.ASTRegexOT(first) {

  def translate = value
}


object IdentifierT extends IdentifierT(new ObjectTranslators.IdentityOT[Identifier])

class IdentifierT[T<:ASTObject](first: ObjectTranslators.OT[T, Identifier]) extends ot.IdentifierOT(first) with Util {

  def translate = value
  def referencedStatement = find(value).getOrFail(format("Group element reference '%s' not found",value),pos)
  def inGroup = referencedStatement.isObject.ifTrueElse(
    format("""%s.T("")""",value.refname),
    format("""%s.ifTrueElse("%s","")""",value.refname,value)
  )
}

object KeywordStatementT extends KeywordStatementT(new ObjectTranslators.IdentityOT[KeywordStatement])

class KeywordStatementT[T](first: ObjectTranslators.OT[T, KeywordStatement]) extends ot.KeywordStatementOT(first) with Util {

  def translate = C("")
}


object KeywordParametersT extends KeywordParametersT(new ObjectTranslators.IdentityOT[KeywordParameters])

class KeywordParametersT[T](first: ObjectTranslators.OT[T, KeywordParameters]) extends ot.KeywordParametersOT(first) {
  def translate = C("")
}


object StringRegexT extends StringRegexT(new ObjectTranslators.IdentityOT[StringRegex])

class StringRegexT[T](first: ObjectTranslators.OT[T, StringRegex]) extends ot.StringRegexOT(first) {

  def translate = string.T("") +
    o_regex.T("")
}


object TokenStatementT extends TokenStatementT(new ObjectTranslators.IdentityOT[TokenStatement])

class TokenStatementT[T](first: ObjectTranslators.OT[T, TokenStatement]) extends ot.TokenStatementOT(first) with Util {

  def translate =format("""
                          |class  %s[T](first:OT[T,%s])
                          |extends ot.%s(first) with Utils{
                          |
                          |  def translate = this/{ x => '"'+x.value.dropWhile(_ == '"').reverse.dropWhile(_ == '"').reverse+'"'}
                          |}
                          |
                          |""".stripMargin,
    name.T.ottname,name.T.objname,name.T.otname)

}


object IgnoreStatementT extends IgnoreStatementT(new ObjectTranslators.IdentityOT[IgnoreStatement])

class IgnoreStatementT[T](first: ObjectTranslators.OT[T, IgnoreStatement]) extends ot.IgnoreStatementOT(first) {

  def translate = C("")
}


object RuleStatementT extends RuleStatementT(new ObjectTranslators.IdentityOT[RuleStatement])

class RuleStatementT[T](first: ObjectTranslators.OT[T, RuleStatement]) extends ot.RuleStatementOT(first) with Util {

  def translate =format("""
                          |class  %s[T](first:OT[T,%s])
                          |extends ot.%s(first) with Utils{
                          |
                          |  def translate = C("%s(") + %s + C(")")
                          |}
                          |
                          |""".stripMargin,
    name.T.ottname,name.T.objname,name.T.otname,
    name.T.toLowerCamelCase,
    sequence.join(""" + C(",\n  ") +"""))
}


object RuleElementT extends RuleElementT(new ObjectTranslators.IdentityOT[RuleElement])

class RuleElementT[T](first: ObjectTranslators.OT[T, RuleElement]) extends ot.RuleElementOT(first) with Util {

  def translate = stringRuleElement.T("") +
    patternRuleElement.T("")
}


object RegisterT extends RegisterT(new ObjectTranslators.IdentityOT[Register])

class RegisterT[T](first: ObjectTranslators.OT[T, Register]) extends ot.RegisterOT(first) {

  def translate = C("")
}


object StringRuleElementT extends StringRuleElementT(new ObjectTranslators.IdentityOT[StringRuleElement])

class StringRuleElementT[T](first: ObjectTranslators.OT[T, StringRuleElement]) extends ot.StringRuleElementOT(first) {

  def translate = C("")
}


object PatternRuleElementT extends PatternRuleElementT(new ObjectTranslators.IdentityOT[PatternRuleElement])

class PatternRuleElementT[T <: ASTObject](first: ObjectTranslators.OT[T, PatternRuleElement]) extends ot.PatternRuleElementOT(first) with Util {
  def translate = referencedStatement.keywordStatement.ifDefinedOrElse(
    multiplicity.ifDefinedOrElse(name.T.refname + multiplicity.get/MultiplicityT.keywordMultiplicity ,C("")),
    multiplicity.ifDefinedOrElse(
      C("""C("[\n") + """)+name.T.refname + multiplicity.get.T+C(""" + C("\n]")"""),
      name.T.refname + C(".T")
    )
  )
  def referencedStatement = find(ref).getOrFail(format("Reference %s not found", ref), pos)
  def reference = ref.refname
  def ref = elementReference.ifDefinedOrElse(elementReference.get.name.value, name.value)
}


object ElementReferenceT extends ElementReferenceT(new ObjectTranslators.IdentityOT[ElementReference])

class ElementReferenceT[T](first: ObjectTranslators.OT[T, ElementReference]) extends ot.ElementReferenceOT(first) with Util {

  def translate = C("")
}


object SplitByT extends SplitByT(new ObjectTranslators.IdentityOT[SplitBy])

class SplitByT[T](first: ObjectTranslators.OT[T, SplitBy]) extends ot.SplitByOT(first) {

  def translate = C("")
}


object OneOrMoreT extends OneOrMoreT(new ObjectTranslators.IdentityOT[OneOrMore])

class OneOrMoreT[T](first: ObjectTranslators.OT[T, OneOrMore]) extends ot.OneOrMoreOT(first) with Util {

  def translate = C(""".join(",\n").indent""")
}


object MoreT extends MoreT(new ObjectTranslators.IdentityOT[More])

class MoreT[T](first: ObjectTranslators.OT[T, More]) extends ot.MoreOT(first) with Util {

  def translate = C(""".join(",\n").indent""")
}


object MultiplicityT extends MultiplicityT(new ObjectTranslators.IdentityOT[Multiplicity])

class MultiplicityT[T <: ASTObject](first: ObjectTranslators.OT[T, Multiplicity]) extends ot.MultiplicityOT(first) with Util {

  def translate = option.ifTrueElse(C(""".T("")"""),C("")).str +
    oneOrMore.T("") +
    more.T("")
  def keywordMultiplicity = option.ifTrueElse(C(""".ifTrueElse(C("true"),C("false"))"""),C("")).str
    //oneOrMore.ifDefinedOrElse(error("Keyword '+' multiplicity not supported inside a rule."),C("") )+
    //more.ifDefinedOrElse(error("Keyword '*' multiplicity not supported inside a rule."),C("") )
}


object BinaryStatementT extends BinaryStatementT(new ObjectTranslators.IdentityOT[BinaryStatement])

class BinaryStatementT[T](first: ObjectTranslators.OT[T, BinaryStatement]) extends ot.BinaryStatementOT(first) {

  def translate = C("//binary") + name.T + C("on") + operand.T + C("(") + sequence.join(",") + C(")")
}


object GroupStatementT extends GroupStatementT(new ObjectTranslators.IdentityOT[GroupStatement])

class GroupStatementT[T](first: ObjectTranslators.OT[T, GroupStatement]) extends todefaulttranslator.GroupStatementT[T](first)

object StatementT extends StatementT(new ObjectTranslators.IdentityOT[Statement])

class StatementT[T](first: ObjectTranslators.OT[T, Statement]) extends todefaulttranslator.StatementT[T](first)


object MainT extends MainT(new ObjectTranslators.IdentityOT[Main])

class MainT[T](first: ObjectTranslators.OT[T, Main]) extends ot.MainOT(first) {

  def translate = format( """package %s.prologgenerator
                            |import eu.lateral.nomic.ObjectTranslators
                            |import eu.lateral.nomic.ObjectTranslators.OT
                            |import %s.ast._
                            |import %s.ot
                            |
                            |trait Utils{
                            |%s
                            |}
                            |
                            |%s
                            |%s
                            |
                            |class PrologGenerator extends ObjectTranslators.TranslatorWithProperties {
                            |  override def apply(obj:Any):String = obj match{
                            |%s
                            |    case _ => obj.toString
                            |  }
                            |
                            |}
                            |
                          """.stripMargin,
    translatorProperty("package"), translatorProperty("package"), translatorProperty("package"),
    sequence.map(StatementT.implicitConversion).join("\n"),
    sequence.map(StatementT.identityObject).join("\n"),
    sequence.join(""),
    sequence.map(StatementT.cases).join(""))
}


class ToPrologGenerator extends ObjectTranslators.TranslatorWithProperties {

  override def apply(obj: Any): String = obj match {
    case x: ASTString => ASTStringT.translate(this, x)
    case x: ASTRegex => ASTRegexT.translate(this, x)
    case x: Identifier => IdentifierT.translate(this, x)
    case x: KeywordStatement => KeywordStatementT.translate(this, x)
    case x: KeywordParameters => KeywordParametersT.translate(this, x)
    case x: StringRegex => StringRegexT.translate(this, x)
    case x: TokenStatement => TokenStatementT.translate(this, x)
    case x: IgnoreStatement => IgnoreStatementT.translate(this, x)
    case x: RuleStatement => RuleStatementT.translate(this, x)
    case x: RuleElement => RuleElementT.translate(this, x)
    case x: Register => RegisterT.translate(this, x)
    case x: StringRuleElement => StringRuleElementT.translate(this, x)
    case x: PatternRuleElement => PatternRuleElementT.translate(this, x)
    case x: ElementReference => ElementReferenceT.translate(this, x)
    case x: SplitBy => SplitByT.translate(this, x)
    case x: OneOrMore => OneOrMoreT.translate(this, x)
    case x: More => MoreT.translate(this, x)
    case x: Multiplicity => MultiplicityT.translate(this, x)
    case x: BinaryStatement => BinaryStatementT.translate(this, x)
    case x: GroupStatement => GroupStatementT.translate(this, x)
    case x: Statement => StatementT.translate(this, x)
    case x: Main => MainT.translate(this, x)

    case _ => obj.toString
  }

}
