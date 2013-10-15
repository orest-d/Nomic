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

package eu.lateral.nomic.meno.ottranslator

import eu.lateral.nomic.meno.{parsertranslator, Util, ot}
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.ObjectTranslators
import ObjectTranslators.{OT, StringOT, Translator}
import eu.lateral.nomic.ASTObjects.ASTObject

object ASTStringT extends ASTStringT(new ObjectTranslators.IdentityOT[ASTString])

class ASTStringT[T](first: ObjectTranslators.OT[T, ASTString]) extends ot.ASTStringOT(first) {

  def translate = value
}


object ASTRegexT extends ASTRegexT(new ObjectTranslators.IdentityOT[ASTRegex])

class ASTRegexT[T](first: ObjectTranslators.OT[T, ASTRegex]) extends ot.ASTRegexOT(first) {

  def translate = value
}


object IdentifierT extends IdentifierT(new ObjectTranslators.IdentityOT[Identifier])

class IdentifierT[T<:ASTObject](first: ObjectTranslators.OT[T, Identifier]) extends ot.IdentifierOT(first) with Util {

  def translate = value
  def objType = ancestor[GroupStatement].get/GroupStatementT.name.T.objname
  def referenced =find(value)
  def groupMember=referenced.ifDefinedOrElse(
    format(
      """
        | def %-20s = new %s(
        |                              this/((_:%s).%s)%s
        |                            )
        |""".stripMargin,
      referenced.get/StatementT.name.refname,
      referenced.get/StatementT.objType,
      objType,
      referenced.get/StatementT.name.refname,
      referenced.get/StatementT.chaining
    ),
    C("")
  )
/*
  def insideGroup = format("%-20s ^^ (%s(_ :ASTObjects.ASTObject))",
    value.refname,
    (ancestor[GroupStatement].get / GroupStatementT.name.T.objname).str)
    */
}

/*
object NamedObjectT extends NamedObjectT(new ObjectTranslators.IdentityOT[NamedObject])

class NamedObjectT[T](first: ObjectTranslators.OT[T, NamedObject]) extends ot.NamedObjectOT(first) {

  def translate = name.T
}


object ClassGeneratorT extends ClassGeneratorT(new ObjectTranslators.IdentityOT[ClassGenerator])

class ClassGeneratorT[T](first: ObjectTranslators.OT[T, ClassGenerator]) extends ot.ClassGeneratorOT(first) {

  def translate = name.T
}
*/

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

  def translate =format("class %s[T](first:ObjectTranslators.OT[T,%s]) extends ObjectTranslators.LiteralOT(first)\n\n",
        name.T.otname,name.T.objname)
}


object IgnoreStatementT extends IgnoreStatementT(new ObjectTranslators.IdentityOT[IgnoreStatement])

class IgnoreStatementT[T](first: ObjectTranslators.OT[T, IgnoreStatement]) extends ot.IgnoreStatementOT(first) {

  def translate = C("")
}


object RuleStatementT extends RuleStatementT(new ObjectTranslators.IdentityOT[RuleStatement])

class RuleStatementT[T](first: ObjectTranslators.OT[T, RuleStatement]) extends ot.RuleStatementOT(first) with Util {

  def translate =format("""|class %s[T](first:ObjectTranslators.OT[T,%s]) extends ObjectTranslators.ASTObjectOT(first){
                           |%s
                           |}
                           |""".stripMargin, name.T.otname,name.T.objname, sequence.join.indent)
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
  def objType =ancestor[RuleStatement].get/RuleStatementT.name.T.objname
  def translate = referencedStatement.isObject.ifTrueElse(
    extractor,
    referencedStatement.keywordStatement.ifDefinedOrElse(flagExtractor,C("//Error"))
  )
  def extractor = multiplicity.ifDefinedOrElse(
    multiplicity.get/MultiplicityT.extractor,
    format("def %-20s = new %s(this/((_:%s).%s))\n",
      name.T.refname,
      basicElementType.otname,
      objType,
      name.T.refname)
  )
  def flagExtractor = multiplicity.ifDefinedOrElse(multiplicity.get/MultiplicityT.flagExtractor,C(""))

  def referencedStatement = find(ref).getOrFail(format("Reference %s not found", ref), pos)
  def reference = ref.refname

  def ref = elementReference.ifDefinedOrElse(elementReference.get.name.value, name.value)

  def basicElementType = ref.objname

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

  def translate = C("")
}


object MoreT extends MoreT(new ObjectTranslators.IdentityOT[More])

class MoreT[T](first: ObjectTranslators.OT[T, More]) extends ot.MoreOT(first) with Util {

  def translate = C("")
}


object MultiplicityT extends MultiplicityT(new ObjectTranslators.IdentityOT[Multiplicity])

class MultiplicityT[T <: ASTObject](first: ObjectTranslators.OT[T, Multiplicity]) extends ot.MultiplicityOT(first) with Util {

  def translate = option.ifTrueElse(C("Option"),C("")).str +
    oneOrMore.ifDefinedOrElse(C("List"),C("")).str +
    more.ifDefinedOrElse(C("List"),C("")).str

  def name =   ancestor[PatternRuleElement].get/PatternRuleElementT.name.T
  def basicElementType =   ancestor[PatternRuleElement].get/PatternRuleElementT.basicElementType
  def objType =ancestor[RuleStatement].get/RuleStatementT.name.T.objname
  def flagDeclaration = option.ifTrueElse(name.refname+C(" : Boolean"),C(""))
  def flagExtractor=option.ifTrueElse(
    format("def %-20s = new ObjectTranslators.BooleanOT(this/((_:%s).%s))\n",name.refname,objType,name.refname),
    C("")
  )

  def extractor= option.ifTrueElse(
      format("""|def %-20s = new ObjectTranslators.ChainingOptionOT(
                |                             this/((_:%s).%s),
                |                             (c:ObjectTranslators.OT[T,%s])=>new %s[T](c)
                |                           )
                |""".stripMargin,
        name.refname,objType,name.refname,
        basicElementType,basicElementType.otname),
      C("")
    ).str +
    oneOrMore.ifDefinedOrElse(
      format("def %-20s = new ObjectTranslators.ListOT(this/((_:%s).%s.list))\n",name.refname,objType,name.refname),
      C("")
    ).str +
    more.ifDefinedOrElse(
      format("def %-20s = new ObjectTranslators.ListOT(this/((_:%s).%s.list))\n",name.refname,objType,name.refname),
      C("")
    ).str
}


object BinaryStatementT extends BinaryStatementT(new ObjectTranslators.IdentityOT[BinaryStatement])

class BinaryStatementT[T](first: ObjectTranslators.OT[T, BinaryStatement]) extends ot.BinaryStatementOT(first) {

  def translate = C("//binary") + name.T + C("on") + operand.T + C("(") + sequence.join(",") + C(")")
}


object GroupStatementT extends GroupStatementT(new ObjectTranslators.IdentityOT[GroupStatement])

class GroupStatementT[T](first: ObjectTranslators.OT[T, GroupStatement]) extends ot.GroupStatementOT(first) with Util {

  def translate = format("class %s[T](first:ObjectTranslators.OT[T,%s]) extends ObjectTranslators.ASTObjectOT(first){\n%s\n}\n",
    name.T.otname, name.T.objname, sequence.map(IdentifierT.groupMember).join("") )
}


object StatementT extends StatementT(new ObjectTranslators.IdentityOT[Statement])

class StatementT[T](first: ObjectTranslators.OT[T, Statement]) extends ot.StatementOT(first) with Util{

  def translate = tokenStatement.T("") +
    keywordStatement.T("") +
    ruleStatement.T("") +
    groupStatement.T("") +
    ignoreStatement.T("") +
    binaryStatement.T("")
  def name =
    tokenStatement.ifDefinedOrElse(tokenStatement.get.name.T,C("")) +
    keywordStatement.ifDefinedOrElse(keywordStatement.get.name.T,C("")) +
    ruleStatement.ifDefinedOrElse(ruleStatement.get.name.T,C("")) +
    groupStatement.ifDefinedOrElse(groupStatement.get.name.T,C("")) +
    binaryStatement.ifDefinedOrElse(binaryStatement.get.name.T,C(""))
  def objType =
    tokenStatement.ifDefinedOrElse(C("ObjectTranslators.ChainingOptionOT"),C("")) +
    keywordStatement.ifDefinedOrElse(C("ObjectTranslators.BooleanOT"),C("")) +
    ruleStatement.ifDefinedOrElse(C("ObjectTranslators.ChainingOptionOT"),C(""))
  def chaining =
    tokenStatement.ifDefinedOrElse(
      format(",\n                              (c:ObjectTranslators.OT[T,%s])=>new %s[T](c)",
        name.T.objname,name.T.otname).str,C("")) +
    ruleStatement.ifDefinedOrElse(
      format(",\n                              (c:ObjectTranslators.OT[T,%s])=>new %s[T](c)",
        name.T.objname,name.T.otname).str,C(""))
}


object MainT extends MainT(new ObjectTranslators.IdentityOT[Main])

class MainT[T](first: ObjectTranslators.OT[T, Main]) extends ot.MainOT(first) {

  def translate = format( """package %s.ot
                            |import eu.lateral.nomic.ObjectTranslators
                            |import %s.ast._
                            |
                            |%s
                            |""".stripMargin, translatorProperty("package"), translatorProperty("package"), sequence.join("\n \n"))
}


class OTTranslator extends ObjectTranslators.TranslatorWithProperties {

  override def apply(obj: Any): String = obj match {
    case x: ASTString => ASTStringT.translate(this, x)
    case x: ASTRegex => ASTRegexT.translate(this, x)
    case x: Identifier => IdentifierT.translate(this, x)
    //case x: NamedObject => NamedObjectT.translate(this, x)
    //case x: ClassGenerator => ClassGeneratorT.translate(this, x)
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

object TranslatorTest {
  def main(arg: Array[String]) {
    println("Hello from translator")
    test
  }

  def test {
    val ast = eu.lateral.nomic.meno.parser.Parser.fromFile("/home/orest/zlos/MenoScala/scala-nomic-meno/src/main/resources/meno.meno")
    println("AST:  " + ast)
    println("TRANS:" + (new OTTranslator).apply(ast))
  }
}
