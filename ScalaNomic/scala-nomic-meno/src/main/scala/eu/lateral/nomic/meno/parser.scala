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

package eu.lateral.nomic.meno.parser

import java.io.File
import org.apache.commons.io.FileUtils.readFileToString
import eu.lateral.nomic.meno.ast._
import scala.Responder
import scala.util.parsing.combinator.RegexParsers
import util.parsing.input.CharArrayReader
import eu.lateral.nomic.ASTObjects

object Parser extends RegexParsers{
  override def skipWhitespace = true
  override val whiteSpace = """(\s+)|(#.*)""".r
  override def handleWhiteSpace(source: java.lang.CharSequence, offset: Int): Int =
    if (skipWhitespace)
      (whiteSpace findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) => handleWhiteSpace(source,offset + matched.end)
        case None => offset
      }
    else
      offset
  
  def keyword            = literal("keyword") ^^ (_ => Keyword)

  def token              = literal("token") ^^ (_ => Token)

  def rule               = literal("rule") ^^ (_ => Rule)

  def group              = literal("group") ^^ (_ => Group)

  def ignore             = literal("ignore") ^^ (_ => Ignore)

  def option             = literal("?") ^^ (_ => ASTOption)

  def positionMark       = literal("@") ^^ (_ => PositionMark)

  def binary             = literal("binary") ^^ (_ => Binary)

  def string             = positioned(regex("\"[^\"\\\\\\r\\n]*(?:\\\\.[^\"\\\\\\r\\n]*)*\""r) ^^ {ASTString(_:String)})

  def o_regex            = positioned(regex("/.*/"r) ^^ {ASTRegex(_:String)})

  def identifier         = positioned(regex("[a-zA-Z_][a-zA-Z0-9_]*"r) ^^ {Identifier(_:String)})

  def namedObject        = positioned(
    identifier ^^ {
      case name => NamedObject(name)
    })

  def classGenerator     = positioned(
    identifier ^^ {
      case name => ClassGenerator(name)
    })

  def keywordStatement   = positioned(
    keyword ~ identifier ~ opt(keywordParameters) ^^ {
      case keyword ~ name ~ keywordParameters => KeywordStatement(name, keywordParameters)
    })

  def keywordParameters  = positioned(
    string ~ opt(stringRegex) ^^ {
      case string ~ o_regex => KeywordParameters(string, o_regex)
    })

  def stringRegex        = positioned(
                             string               ^^ (StringRegex(_ :ASTObjects.ASTObject)) |
                             o_regex              ^^ (StringRegex(_ :ASTObjects.ASTObject))
                           )

  def tokenStatement     = positioned(
    token ~ identifier ~ stringRegex ~ rep(string) ^^ {
      case token ~ name ~ o_regex ~ test => TokenStatement(name, o_regex, test)
    })

  def ignoreStatement    = positioned(
    ignore ~ stringRegex ^^ {
      case ignore ~ o_regex => IgnoreStatement(o_regex)
    })

  def ruleStatement      = positioned(
    rule ~ identifier ~ "(" ~ repsep(ruleElement,",") ~ ")" ^^ {
      case rule ~ name ~ _ ~ sequence ~ _ => RuleStatement(name, sequence)
    })

  def ruleElement        = positioned(
                             stringRuleElement    ^^ (RuleElement(_ :ASTObjects.ASTObject)) |
                             patternRuleElement   ^^ (RuleElement(_ :ASTObjects.ASTObject))
                           )

  def register           = positioned(
    "!" ~ identifier ^^ {
      case _ ~ identifier => Register(identifier)
    })

  def stringRuleElement  = positioned(
    opt(positionMark) ~ string ~ opt(option) ^^ {
      case positionMark ~ string ~ option => StringRuleElement(positionMark isDefined, string, option isDefined)
    })

  def patternRuleElement = positioned(
    opt(positionMark) ~ identifier ~ opt(elementReference) ~ opt(multiplicity) ~ opt(register) ^^ {
      case positionMark ~ name ~ elementReference ~ multiplicity ~ register => PatternRuleElement(positionMark isDefined, name, elementReference, multiplicity, register)
    })

  def elementReference   = positioned(
    ":" ~ identifier ^^ {
      case _ ~ name => ElementReference(name)
    })

  def splitBy            = positioned(
    "(" ~ string ~ ")" ^^ {
      case _ ~ string ~ _ => SplitBy(string)
    })

  def oneOrMore          = positioned(
    "+" ~ opt(splitBy) ^^ {
      case _ ~ splitBy => OneOrMore(splitBy)
    })

  def more               = positioned(
    "*" ~ opt(splitBy) ^^ {
      case _ ~ splitBy => More(splitBy)
    })

  def multiplicity       = positioned(
                             option               ^^ (Multiplicity(_ :ASTObjects.ASTObject)) |
                             oneOrMore            ^^ (Multiplicity(_ :ASTObjects.ASTObject)) |
                             more                 ^^ (Multiplicity(_ :ASTObjects.ASTObject))
                           )

  def binaryStatement    = positioned(
    binary ~ identifier ~ "on" ~ identifier ~ "(" ~ rep1sep(identifier,",") ~ ")" ^^ {
      case binary ~ name ~ _ ~ operand ~ _ ~ sequence ~ _ => BinaryStatement(name, operand, sequence)
    })

  def groupStatement     = positioned(
    group ~ identifier ~ "(" ~ rep1sep(identifier,",") ~ ")" ^^ {
      case group ~ name ~ _ ~ sequence ~ _ => GroupStatement(name, sequence)
    })

  def statement          = positioned(
                             tokenStatement       ^^ (Statement(_ :ASTObjects.ASTObject)) |
                             keywordStatement     ^^ (Statement(_ :ASTObjects.ASTObject)) |
                             ruleStatement        ^^ (Statement(_ :ASTObjects.ASTObject)) |
                             groupStatement       ^^ (Statement(_ :ASTObjects.ASTObject)) |
                             ignoreStatement      ^^ (Statement(_ :ASTObjects.ASTObject)) |
                             binaryStatement      ^^ (Statement(_ :ASTObjects.ASTObject))
                           )

  def main               = positioned(
    rep(statement) ^^ {
      case sequence => Main(sequence)
    })


  def apply(txt:String)={
    phrase(main)(new CharArrayReader(txt.toArray)) match{
      case Success(x,_)=>x
      case e:NoSuccess => println(e)
    }
  }

  def fromFile(path:String) = {
    val txt = readFileToString(new File(path))
    apply(txt)
  }

  implicit def convert_option[T](input:Option[AnyRef]):Option[T] = input.map(_.asInstanceOf[T])

  implicit def convert_list[T,Elem<:ASTObjects.ASTObject](input:List[AnyRef]):ASTObjects.AList[Elem] = {
    new ASTObjects.AList(input.map(_.asInstanceOf[Elem]))
  }
}

object ParserTest {
  def main(arg:Array[String]){
    println("Hello")
    test
  }
  def test{
    println("test: "+Parser("token id /[a-z]+/\n"))
  }
}
