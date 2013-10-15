/*
This file is part of Scala Nomic Runtime.

    Scala Nomic Runtime is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scala Nomic Runtime is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Scala Nomic Runtime.  If not, see <http://www.gnu.org/licenses/>.
*/

package eu.lateral.nomic

object TextUtils {
  def isAllCapital(s:String):Boolean = (s==s.toUpperCase)

  def idToList(text:String):List[String]={
    val nisac=(! isAllCapital(text))
    var result:List[String]=Nil
    var token=""
      
    for (c <- text){
      if (!c.isLetterOrDigit || (nisac && c.isUpper )){
        if (token.length>0){
          result=token::result
          token=""
        }
      }
      if (c isLetterOrDigit){
        token+=c
      }
    }
    if (token.length>0){
      result=token::result      
    }
    result.reverse
  }

  def upperCamelCase(text:String):String=idToList(text).map(_.toLowerCase.capitalize).mkString

  def lowerCamelCase(text:String):String=idToList(text) match{
    case Nil        => ""
    case head::Nil  => head.toLowerCase
    case head::tail => head.toLowerCase + tail.map(_.toLowerCase.capitalize).mkString
  }
  def allCaps(text:String,sep:String)=idToList(text).map(_.toUpperCase).mkString(sep)
  def allCaps(text:String):String=allCaps(text,"_")
  def smallCaps(text:String,sep:String):String=idToList(text).map(_.toLowerCase).mkString(sep)
  def smallCaps(text:String):String=smallCaps(text,"_")
  def joinText(text:String,sep:String):String=idToList(text).mkString(sep)
  def joinText(text:String):String=joinText(text," ")
  def indent(text:String,prefix:String):String={
    val l=for (x <- exactSplit(text,'\n')) yield
      if (x.length==0)
        x
      else
        prefix+x
    l.mkString("\n") 
  }
  def indent(text:String):String=indent(text,"  ")
  def indent(text:String,prefix:Int):String=indent(text,"  "*prefix)
  def exactSplit(text:String,separator:Char):List[String]={
    var result:List[String]=Nil
    var token=""
      
    for (c <- text){
      if (c==separator){
        if (token.length>0){
          result=token::result
          token=""
        }
      }
      else{
        token+=c
      }
    }
    result=token::result      
    result.reverse
  }
  def escape(text:String):String={
    text.replace("\\","\\\\").
    replace("\n","\\n").
    replace("\r","\\r").
    replace("\t","\\t").
    replace("\'","\\\'").
    replace("\"","\\\"")
  }

}