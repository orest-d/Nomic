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

package eu.lateral.nomic.meno.prologgenerator
import eu.lateral.nomic.ObjectTranslators
import eu.lateral.nomic.ObjectTranslators.OT
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.meno.ot

trait Utils{
  implicit def toASTStringT[T](x:OT[T,ASTString]):ASTStringT[T] =
    new ASTStringT(x)
  implicit def toASTRegexT[T](x:OT[T,ASTRegex]):ASTRegexT[T] =
    new ASTRegexT(x)
  implicit def toIdentifierT[T](x:OT[T,Identifier]):IdentifierT[T] =
    new IdentifierT(x)
  implicit def toNamedObjectT[T](x:OT[T,NamedObject]):NamedObjectT[T] =
    new NamedObjectT(x)
  implicit def toClassGeneratorT[T](x:OT[T,ClassGenerator]):ClassGeneratorT[T] =
    new ClassGeneratorT(x)
  implicit def toKeywordStatementT[T](x:OT[T,KeywordStatement]):KeywordStatementT[T] =
    new KeywordStatementT(x)
  implicit def toKeywordParametersT[T](x:OT[T,KeywordParameters]):KeywordParametersT[T] =
    new KeywordParametersT(x)
  implicit def toStringRegexT[T](x:OT[T,StringRegex]):StringRegexT[T] =
    new StringRegexT(x)
  implicit def toTokenStatementT[T](x:OT[T,TokenStatement]):TokenStatementT[T] =
    new TokenStatementT(x)
  implicit def toIgnoreStatementT[T](x:OT[T,IgnoreStatement]):IgnoreStatementT[T] =
    new IgnoreStatementT(x)
  implicit def toRuleStatementT[T](x:OT[T,RuleStatement]):RuleStatementT[T] =
    new RuleStatementT(x)
  implicit def toRuleElementT[T](x:OT[T,RuleElement]):RuleElementT[T] =
    new RuleElementT(x)
  implicit def toRegisterT[T](x:OT[T,Register]):RegisterT[T] =
    new RegisterT(x)
  implicit def toStringRuleElementT[T](x:OT[T,StringRuleElement]):StringRuleElementT[T] =
    new StringRuleElementT(x)
  implicit def toPatternRuleElementT[T](x:OT[T,PatternRuleElement]):PatternRuleElementT[T] =
    new PatternRuleElementT(x)
  implicit def toElementReferenceT[T](x:OT[T,ElementReference]):ElementReferenceT[T] =
    new ElementReferenceT(x)
  implicit def toSplitByT[T](x:OT[T,SplitBy]):SplitByT[T] =
    new SplitByT(x)
  implicit def toOneOrMoreT[T](x:OT[T,OneOrMore]):OneOrMoreT[T] =
    new OneOrMoreT(x)
  implicit def toMoreT[T](x:OT[T,More]):MoreT[T] =
    new MoreT(x)
  implicit def toMultiplicityT[T](x:OT[T,Multiplicity]):MultiplicityT[T] =
    new MultiplicityT(x)
  implicit def toBinaryStatementT[T](x:OT[T,BinaryStatement]):BinaryStatementT[T] =
    new BinaryStatementT(x)
  implicit def toGroupStatementT[T](x:OT[T,GroupStatement]):GroupStatementT[T] =
    new GroupStatementT(x)
  implicit def toStatementT[T](x:OT[T,Statement]):StatementT[T] =
    new StatementT(x)
  implicit def toMainT[T](x:OT[T,Main]):MainT[T] =
    new MainT(x)
}

object ASTStringT extends ASTStringT(new ObjectTranslators.IdentityOT[ASTString])
object ASTRegexT extends ASTRegexT(new ObjectTranslators.IdentityOT[ASTRegex])
object IdentifierT extends IdentifierT(new ObjectTranslators.IdentityOT[Identifier])
object NamedObjectT extends NamedObjectT(new ObjectTranslators.IdentityOT[NamedObject])
object ClassGeneratorT extends ClassGeneratorT(new ObjectTranslators.IdentityOT[ClassGenerator])
object KeywordStatementT extends KeywordStatementT(new ObjectTranslators.IdentityOT[KeywordStatement])
object KeywordParametersT extends KeywordParametersT(new ObjectTranslators.IdentityOT[KeywordParameters])
object StringRegexT extends StringRegexT(new ObjectTranslators.IdentityOT[StringRegex])
object TokenStatementT extends TokenStatementT(new ObjectTranslators.IdentityOT[TokenStatement])
object IgnoreStatementT extends IgnoreStatementT(new ObjectTranslators.IdentityOT[IgnoreStatement])
object RuleStatementT extends RuleStatementT(new ObjectTranslators.IdentityOT[RuleStatement])
object RuleElementT extends RuleElementT(new ObjectTranslators.IdentityOT[RuleElement])
object RegisterT extends RegisterT(new ObjectTranslators.IdentityOT[Register])
object StringRuleElementT extends StringRuleElementT(new ObjectTranslators.IdentityOT[StringRuleElement])
object PatternRuleElementT extends PatternRuleElementT(new ObjectTranslators.IdentityOT[PatternRuleElement])
object ElementReferenceT extends ElementReferenceT(new ObjectTranslators.IdentityOT[ElementReference])
object SplitByT extends SplitByT(new ObjectTranslators.IdentityOT[SplitBy])
object OneOrMoreT extends OneOrMoreT(new ObjectTranslators.IdentityOT[OneOrMore])
object MoreT extends MoreT(new ObjectTranslators.IdentityOT[More])
object MultiplicityT extends MultiplicityT(new ObjectTranslators.IdentityOT[Multiplicity])
object BinaryStatementT extends BinaryStatementT(new ObjectTranslators.IdentityOT[BinaryStatement])
object GroupStatementT extends GroupStatementT(new ObjectTranslators.IdentityOT[GroupStatement])
object StatementT extends StatementT(new ObjectTranslators.IdentityOT[Statement])
object MainT extends MainT(new ObjectTranslators.IdentityOT[Main])

class  ASTStringT[T](first:OT[T,ASTString])
extends ot.ASTStringOT(first) with Utils{

  def translate = this/{ x => '"'+x.value.dropWhile(_ == '"').reverse.dropWhile(_ == '"').reverse+'"'}
}


class  ASTRegexT[T](first:OT[T,ASTRegex])
extends ot.ASTRegexOT(first) with Utils{

  def translate = this/{ x => '"'+x.value.dropWhile(_ == '"').reverse.dropWhile(_ == '"').reverse+'"'}
}


class  IdentifierT[T](first:OT[T,Identifier])
extends ot.IdentifierOT(first) with Utils{

  def translate = this/{ x => '"'+x.value.dropWhile(_ == '"').reverse.dropWhile(_ == '"').reverse+'"'}
}


class  NamedObjectT[T](first:OT[T,NamedObject])
extends ot.NamedObjectOT(first) with Utils{

  def translate = C("namedObject(") + name.T + C(")")
}


class  ClassGeneratorT[T](first:OT[T,ClassGenerator])
extends ot.ClassGeneratorOT(first) with Utils{

  def translate = C("classGenerator(") + name.T + C(")")
}


class  KeywordStatementT[T](first:OT[T,KeywordStatement])
extends ot.KeywordStatementOT(first) with Utils{

  def translate = C("keywordStatement(") + name.T + C(",\n  ") +C("[\n") + keywordParameters.T("") + C("\n]") + C(")")
}


class  KeywordParametersT[T](first:OT[T,KeywordParameters])
extends ot.KeywordParametersOT(first) with Utils{

  def translate = C("keywordParameters(") + string.T + C(",\n  ") +C("[\n") + o_regex.T("") + C("\n]") + C(")")
}


class  StringRegexT[T](first:OT[T,StringRegex])
extends ot.StringRegexOT(first) with Utils{

  def translate = string.T("") +
        o_regex.T("")
}


class  TokenStatementT[T](first:OT[T,TokenStatement])
extends ot.TokenStatementOT(first) with Utils{

  def translate = C("tokenStatement(") + name.T + C(",\n  ") +o_regex.T + C(",\n  ") +C("[\n") + test.join(",\n").indent + C("\n]") + C(")")
}


class  IgnoreStatementT[T](first:OT[T,IgnoreStatement])
extends ot.IgnoreStatementOT(first) with Utils{

  def translate = C("ignoreStatement(") + o_regex.T + C(")")
}


class  RuleStatementT[T](first:OT[T,RuleStatement])
extends ot.RuleStatementOT(first) with Utils{

  def translate = C("ruleStatement(") + name.T + C(",\n  ") +C("[\n") + sequence.join(",\n").indent + C("\n]") + C(")")
}


class  RuleElementT[T](first:OT[T,RuleElement])
extends ot.RuleElementOT(first) with Utils{

  def translate = stringRuleElement.T("") +
        patternRuleElement.T("")
}


class  RegisterT[T](first:OT[T,Register])
extends ot.RegisterOT(first) with Utils{

  def translate = C("register(") + identifier.T + C(")")
}


class  StringRuleElementT[T](first:OT[T,StringRuleElement])
extends ot.StringRuleElementOT(first) with Utils{

  def translate = C("stringRuleElement(") + positionMark.ifTrueElse(C("true"),C("false")) + C(",\n  ") +string.T + C(",\n  ") +option.ifTrueElse(C("true"),C("false")) + C(")")
}


class  PatternRuleElementT[T](first:OT[T,PatternRuleElement])
extends ot.PatternRuleElementOT(first) with Utils{

  def translate = C("patternRuleElement(") + positionMark.ifTrueElse(C("true"),C("false")) + C(",\n  ") +name.T + C(",\n  ") +C("[\n") + elementReference.T("") + C("\n]") + C(",\n  ") +C("[\n") + multiplicity.T("") + C("\n]") + C(",\n  ") +C("[\n") + register.T("") + C("\n]") + C(")")
}


class  ElementReferenceT[T](first:OT[T,ElementReference])
extends ot.ElementReferenceOT(first) with Utils{

  def translate = C("elementReference(") + name.T + C(")")
}


class  SplitByT[T](first:OT[T,SplitBy])
extends ot.SplitByOT(first) with Utils{

  def translate = C("splitBy(") + string.T + C(")")
}


class  OneOrMoreT[T](first:OT[T,OneOrMore])
extends ot.OneOrMoreOT(first) with Utils{

  def translate = C("oneOrMore(") + C("[\n") + splitBy.T("") + C("\n]") + C(")")
}


class  MoreT[T](first:OT[T,More])
extends ot.MoreOT(first) with Utils{

  def translate = C("more(") + C("[\n") + splitBy.T("") + C("\n]") + C(")")
}


class  MultiplicityT[T](first:OT[T,Multiplicity])
extends ot.MultiplicityOT(first) with Utils{

  def translate = option.ifTrueElse("Option","") +
        oneOrMore.T("") +
        more.T("")
}


class  BinaryStatementT[T](first:OT[T,BinaryStatement])
extends ot.BinaryStatementOT(first) with Utils{

  def translate = C("binaryStatement(") + name.T + C(",\n  ") +operand.T + C(",\n  ") +C("[\n") + sequence.join(",\n").indent + C("\n]") + C(")")
}


class  GroupStatementT[T](first:OT[T,GroupStatement])
extends ot.GroupStatementOT(first) with Utils{

  def translate = C("groupStatement(") + name.T + C(",\n  ") +C("[\n") + sequence.join(",\n").indent + C("\n]") + C(")")
}


class  StatementT[T](first:OT[T,Statement])
extends ot.StatementOT(first) with Utils{

  def translate = tokenStatement.T("") +
        keywordStatement.T("") +
        ruleStatement.T("") +
        groupStatement.T("") +
        ignoreStatement.T("") +
        binaryStatement.T("")
}


class  MainT[T](first:OT[T,Main])
extends ot.MainOT(first) with Utils{

  def translate = C("main(") + C("[\n") + sequence.join(",\n").indent + C("\n]") + C(")")
}



class PrologGenerator extends ObjectTranslators.TranslatorWithProperties {
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

                          