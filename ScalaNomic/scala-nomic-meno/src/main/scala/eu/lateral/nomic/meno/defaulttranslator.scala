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

package eu.lateral.nomic.meno.defaulttranslator;

import eu.lateral.nomic.meno.ot
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.ObjectTranslators

//keyword keyword
//keyword token
//keyword rule
//keyword group
//keyword ignore
//keyword option"?"
//keyword PositionMark"@"
//keyword binary

object ASTStringT extends ASTStringT(new ObjectTranslators.IdentityOT[ASTString])
class  ASTStringT[T](first:ObjectTranslators.OT[T,ASTString]) extends ot.ASTStringOT(first){

  def translate = value
}


object ASTRegexT extends ASTRegexT(new ObjectTranslators.IdentityOT[ASTRegex])
class  ASTRegexT[T](first:ObjectTranslators.OT[T,ASTRegex]) extends ot.ASTRegexOT(first){

  def translate = value
}


object IdentifierT extends IdentifierT(new ObjectTranslators.IdentityOT[Identifier])
class  IdentifierT[T](first:ObjectTranslators.OT[T,Identifier]) extends ot.IdentifierOT(first){

  def translate = value
}


object NamedObjectT extends NamedObjectT(new ObjectTranslators.IdentityOT[NamedObject])
class  NamedObjectT[T](first:ObjectTranslators.OT[T,NamedObject]) extends ot.NamedObjectOT(first){

  def translate = name.T
}


object ClassGeneratorT extends ClassGeneratorT(new ObjectTranslators.IdentityOT[ClassGenerator])
class  ClassGeneratorT[T](first:ObjectTranslators.OT[T,ClassGenerator]) extends ot.ClassGeneratorOT(first){

  def translate = name.T
}


object KeywordStatementT extends KeywordStatementT(new ObjectTranslators.IdentityOT[KeywordStatement])
class  KeywordStatementT[T](first:ObjectTranslators.OT[T,KeywordStatement]) extends ot.KeywordStatementOT(first){

  def translate = C("keyword") + name.T + keywordParameters.T("")
}


object KeywordParametersT extends KeywordParametersT(new ObjectTranslators.IdentityOT[KeywordParameters])
class  KeywordParametersT[T](first:ObjectTranslators.OT[T,KeywordParameters]) extends ot.KeywordParametersOT(first){

  def translate = string.T + o_regex.T("")
}


object StringRegexT extends StringRegexT(new ObjectTranslators.IdentityOT[StringRegex])
class  StringRegexT[T](first:ObjectTranslators.OT[T,StringRegex]) extends ot.StringRegexOT(first){

  def translate = string.T("") +
        o_regex.T("")
}


object TokenStatementT extends TokenStatementT(new ObjectTranslators.IdentityOT[TokenStatement])
class  TokenStatementT[T](first:ObjectTranslators.OT[T,TokenStatement]) extends ot.TokenStatementOT(first){

  def translate = C("token") + name.T + o_regex.T + test.join
}


object IgnoreStatementT extends IgnoreStatementT(new ObjectTranslators.IdentityOT[IgnoreStatement])
class  IgnoreStatementT[T](first:ObjectTranslators.OT[T,IgnoreStatement]) extends ot.IgnoreStatementOT(first){

  def translate = C("ignore") + o_regex.T
}


object RuleStatementT extends RuleStatementT(new ObjectTranslators.IdentityOT[RuleStatement])
class  RuleStatementT[T](first:ObjectTranslators.OT[T,RuleStatement]) extends ot.RuleStatementOT(first){

  def translate = C("rule") + name.T + C("(") + sequence.join(",") + C(")")
}


object RuleElementT extends RuleElementT(new ObjectTranslators.IdentityOT[RuleElement])
class  RuleElementT[T](first:ObjectTranslators.OT[T,RuleElement]) extends ot.RuleElementOT(first){

  def translate = stringRuleElement.T("") +
        patternRuleElement.T("")
}


object RegisterT extends RegisterT(new ObjectTranslators.IdentityOT[Register])
class  RegisterT[T](first:ObjectTranslators.OT[T,Register]) extends ot.RegisterOT(first){

  def translate = C("!") + identifier.T
}


object StringRuleElementT extends StringRuleElementT(new ObjectTranslators.IdentityOT[StringRuleElement])
class  StringRuleElementT[T](first:ObjectTranslators.OT[T,StringRuleElement]) extends ot.StringRuleElementOT(first){

  def translate = C("PositionMark") + string.T + C("option")
}


object PatternRuleElementT extends PatternRuleElementT(new ObjectTranslators.IdentityOT[PatternRuleElement])
class  PatternRuleElementT[T](first:ObjectTranslators.OT[T,PatternRuleElement]) extends ot.PatternRuleElementOT(first){

  def translate = C("PositionMark") + name.T + elementReference.T("") + multiplicity.T("") + register.T("")
}


object ElementReferenceT extends ElementReferenceT(new ObjectTranslators.IdentityOT[ElementReference])
class  ElementReferenceT[T](first:ObjectTranslators.OT[T,ElementReference]) extends ot.ElementReferenceOT(first){

  def translate = C(":") + name.T
}


object SplitByT extends SplitByT(new ObjectTranslators.IdentityOT[SplitBy])
class  SplitByT[T](first:ObjectTranslators.OT[T,SplitBy]) extends ot.SplitByOT(first){

  def translate = C("(") + string.T + C(")")
}


object OneOrMoreT extends OneOrMoreT(new ObjectTranslators.IdentityOT[OneOrMore])
class  OneOrMoreT[T](first:ObjectTranslators.OT[T,OneOrMore]) extends ot.OneOrMoreOT(first){

  def translate = C("+") + splitBy.T("")
}


object MoreT extends MoreT(new ObjectTranslators.IdentityOT[More])
class  MoreT[T](first:ObjectTranslators.OT[T,More]) extends ot.MoreOT(first){

  def translate = C("*") + splitBy.T("")
}


object MultiplicityT extends MultiplicityT(new ObjectTranslators.IdentityOT[Multiplicity])
class  MultiplicityT[T](first:ObjectTranslators.OT[T,Multiplicity]) extends ot.MultiplicityOT(first){

  def translate = option.ifTrueElse("Option","") +
        oneOrMore.T("") +
        more.T("")
}


object BinaryStatementT extends BinaryStatementT(new ObjectTranslators.IdentityOT[BinaryStatement])
class  BinaryStatementT[T](first:ObjectTranslators.OT[T,BinaryStatement]) extends ot.BinaryStatementOT(first){

  def translate = C("binary") + name.T + C("on") + operand.T + C("(") + sequence.join(",") + C(")")
}


object GroupStatementT extends GroupStatementT(new ObjectTranslators.IdentityOT[GroupStatement])
class  GroupStatementT[T](first:ObjectTranslators.OT[T,GroupStatement]) extends ot.GroupStatementOT(first){

  def translate = C("Group") + name.T + C("(") + sequence.join(",") + C(")")
}


object StatementT extends StatementT(new ObjectTranslators.IdentityOT[Statement])
class  StatementT[T](first:ObjectTranslators.OT[T,Statement]) extends ot.StatementOT(first){

  def translate = tokenStatement.T("") +
        keywordStatement.T("") +
        ruleStatement.T("") +
        groupStatement.T("") +
        ignoreStatement.T("") +
        binaryStatement.T("")
}


object MainT extends MainT(new ObjectTranslators.IdentityOT[Main])
class  MainT[T](first:ObjectTranslators.OT[T,Main]) extends ot.MainOT(first){

  def translate = sequence.join
}



class DefaultTranslator extends ObjectTranslators.Translator{

  override def apply(obj:Any):String = obj match{
    case x:ASTString            => ASTStringT.translate(this,x)
    case x:ASTRegex             => ASTRegexT.translate(this,x)
    case x:Identifier           => IdentifierT.translate(this,x)
    case x:NamedObject          => NamedObjectT.translate(this,x)
    case x:ClassGenerator       => ClassGeneratorT.translate(this,x)
    case x:KeywordStatement     => KeywordStatementT.translate(this,x)
    case x:KeywordParameters    => KeywordParametersT.translate(this,x)
    case x:StringRegex          => StringRegexT.translate(this,x)
    case x:TokenStatement       => TokenStatementT.translate(this,x)
    case x:IgnoreStatement      => IgnoreStatementT.translate(this,x)
    case x:RuleStatement        => RuleStatementT.translate(this,x)
    case x:RuleElement          => RuleElementT.translate(this,x)
    case x:Register             => RegisterT.translate(this,x)
    case x:StringRuleElement    => StringRuleElementT.translate(this,x)
    case x:PatternRuleElement   => PatternRuleElementT.translate(this,x)
    case x:ElementReference     => ElementReferenceT.translate(this,x)
    case x:SplitBy              => SplitByT.translate(this,x)
    case x:OneOrMore            => OneOrMoreT.translate(this,x)
    case x:More                 => MoreT.translate(this,x)
    case x:Multiplicity         => MultiplicityT.translate(this,x)
    case x:BinaryStatement      => BinaryStatementT.translate(this,x)
    case x:GroupStatement       => GroupStatementT.translate(this,x)
    case x:Statement            => StatementT.translate(this,x)
    case x:Main                 => MainT.translate(this,x)

    case _ => obj.toString
  }

}

object TranslatorTest {
  def main(arg:Array[String]){
    println("Hello from translator")
    test
  }
  def test{
    val ast=eu.lateral.nomic.meno.parser.Parser("token id /[a-z]+/\n")
    println("AST:  "+ast)
    println("TRANS:"+(new DefaultTranslator).apply(ast))
  }
}
