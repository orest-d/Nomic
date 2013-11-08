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

package eu.lateral.nomic.meno
import eu.lateral.nomic.meno.ast._
import eu.lateral.nomic.ObjectTranslators
import eu.lateral.nomic.TextUtils._
import eu.lateral.nomic.ASTObjects.ASTObject
import ObjectTranslators.{OT, StringOT, Translator}
import util.parsing.input.Position
import eu.lateral.nomic.errors.PositionError
import eu.lateral.nomic.meno.asttranslator.StatementT

class StringConventionOT[T](first:ObjectTranslators.OT[T,String]) extends ObjectTranslators.StringOT[T](first) with Util{
  def ucc:ObjectTranslators.StringOT[T]=new StringOT(this / (upperCamelCase _))
  def lcc:ObjectTranslators.StringOT[T]=new StringOT(this / (lowerCamelCase _))
  def refname:ObjectTranslators.StringOT[T]=new StringOT(this / (x=>{
    val s=lowerCamelCase(x)
    if (List("literal","regex").contains(s)) "o_"+s else s
  }))
  def objname:ObjectTranslators.StringOT[T]=new StringOT(this / (x=>{
    val s=upperCamelCase(x)
    if (List("Regex","String","Option").contains(s)) "AST"+s else s
  }))
  def otname:ObjectTranslators.StringOT[T]=objname + C("OT")
  def ottname:ObjectTranslators.StringOT[T]=objname + C("T")
  def otiname:ObjectTranslators.StringOT[T]=objname + C("T")
}

trait Util{
  implicit def toStringConventionOT[T](x:ObjectTranslators.OT[T,String])=new StringConventionOT(x)
  def nsMap(m:Main):Map[String,ASTObject]={
    for (x<-m.sequence.list) yield{
      (for (xx <- x.binaryStatement) yield xx.name.value -> xx) ++
        (for (xx <- x.groupStatement ) yield xx.name.value -> xx) ++
        (for (xx <- x.keywordStatement) yield xx.name.value -> xx) ++
        (for (xx <- x.tokenStatement) yield xx.name.value -> xx)
    }
  }.flatten.toMap

  def find(obj:ASTObject,name:String):Option[ASTObject]={
    for (main <- obj.ancestor[Main];obj<-nsMap(main).get(name)) yield obj
  }

  def findAllIgnores(obj:ASTObject):List[IgnoreStatement]={
    for (
      main <- obj.ancestorOrSelf[Main].toList ;
      statement <- main.sequence.list ;
      ignore <- statement.ignoreStatement 
    ) yield  ignore
  }
  
  def findKeyword(obj:ASTObject,name:String):Option[KeywordStatement]={
    for (
      main <- obj.ancestor[Main].toList ;
      statement <- main.sequence.list ;
      keyword <- statement.keywordStatement ;
      if (lowerCamelCase(keyword.name.value) == lowerCamelCase(name))
    ) yield keyword
  }.headOption

  def findToken(obj:ASTObject,name:String):Option[TokenStatement]={
    for (
      main <- obj.ancestor[Main].toList ;
      statement <- main.sequence.list ;
      token <- statement.tokenStatement ;
      if (lowerCamelCase(token.name.value) == lowerCamelCase(name))
    ) yield token
  }.headOption


  def findStatement(obj:ASTObject,name:String):Option[Statement]={
    def lcc(x:String) = lowerCamelCase(x)
    for (
      main <- obj.ancestor[Main].toList ;
      statement <- main.sequence.list;
      if (
        statement.ruleStatement.exists(x => lcc(x.name.value) == lcc(name)) ||
          statement.binaryStatement.exists(x => lcc(x.name.value) == lcc(name)) ||
          statement.groupStatement.exists(x => lcc(x.name.value) == lcc(name)) ||
          statement.tokenStatement.exists(x => lcc(x.name.value) == lcc(name)) ||
          statement.keywordStatement.exists(x => lcc(x.name.value) == lcc(name))
        )
    ) yield statement
  }.headOption

  def find[T<:ASTObject](name:ObjectTranslators.OT[T,String]) = new Find[T](name)
}

class Find[T<:ASTObject](name:ObjectTranslators.OT[T,String]) extends ObjectTranslators.AbstractOptionOT[T,Statement] with Util{
  def apply(translator: Translator, obj: T) = findStatement(obj,name(translator,obj))
  def get = new StatementT(this/(_.get))
  def getOrFail(msg:OT[T,String])=new StatementT[T](F{
    (translator,obj)=>{
      val option = findStatement(obj,name(translator,obj))
      if (option.isDefined)
        option.get
      else{
        translator.error(msg(translator,obj))
        option.get
      }
    }
  })
  def getOrFail(msg:OT[T,String],pos:OT[T,Position])=new StatementT[T](F{
    (translator,obj)=>{
      val option = findStatement(obj,name(translator,obj))
      if (option.isDefined)
        option.get
      else{
        translator.error(PositionError(msg(translator,obj),pos(translator,obj)))
        option.get
      }
    }
  })
}
