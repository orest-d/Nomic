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
  implicit def toRichString(text: String) = new RichString(text)
  implicit def toRichStringSeq(list: Seq[String]) = new RichStringSeq(list)
  
  def isAllCapital(s: String): Boolean = (s == s.toUpperCase)

  def idToList(text: String): List[String] = {
    val nisac = (!isAllCapital(text))
    var result: List[String] = Nil
    var token = ""

    for (c <- text) {
      if (!c.isLetterOrDigit || (nisac && c.isUpper)) {
        if (token.length > 0) {
          result = token :: result
          token = ""
        }
      }
      if (c isLetterOrDigit) {
        token += c
      }
    }
    if (token.length > 0) {
      result = token :: result
    }
    result.reverse
  }

  def upperCamelCase(text: String): String = idToList(text).map(_.toLowerCase.capitalize).mkString

  def lowerCamelCase(text: String): String = idToList(text) match {
    case Nil => ""
    case head :: Nil => head.toLowerCase
    case head :: tail => head.toLowerCase + tail.map(_.toLowerCase.capitalize).mkString
  }
  def allCaps(text: String, sep: String) = idToList(text).map(_.toUpperCase).mkString(sep)
  def allCaps(text: String): String = allCaps(text, "_")
  def smallCaps(text: String, sep: String): String = idToList(text).map(_.toLowerCase).mkString(sep)
  def smallCaps(text: String): String = smallCaps(text, "_")
  def joinText(text: String, sep: String): String = idToList(text).mkString(sep)
  def joinText(text: String): String = joinText(text, " ")
  def indent(text: String, prefix: String): String = {
    val l = for (x <- exactSplit(text, '\n')) yield if (x.length == 0)
      x
    else
      prefix + x
    l.mkString("\n")
  }
  def indent(text: String): String = indent(text, "  ")
  def indent(text: String, prefix: Int): String = indent(text, "  " * prefix)
  def exactSplit(text: String, separator: Char): List[String] = {
    var result: List[String] = Nil
    var token = ""

    for (c <- text) {
      if (c == separator) {
        if (token.length > 0) {
          result = token :: result
          token = ""
        }
      } else {
        token += c
      }
    }
    result = token :: result
    result.reverse
  }
  def escape(text: String): String = {
    text.replace("\\", "\\\\").
      replace("\n", "\\n").
      replace("\r", "\\r").
      replace("\t", "\\t").
      replace("\'", "\\\'").
      replace("\"", "\\\"")
  }

  def join(list: Seq[String], splitBy: String = ""): String = {
    def joinTwo(a: String, b: String) = {
      if (a.length == 0) b else if (b.length == 0) a else a + splitBy + b
    }
    if (list.length == 0) ""
    else if (list.length == 1) list(0)
    else list.reduceLeft(joinTwo)
  }
}

class RichString(val text: String) extends AnyVal {
  def isAllCapital: Boolean = TextUtils.isAllCapital(text)
  def idToList: List[String] = TextUtils.idToList(text)
  def upperCamelCase: String = TextUtils.upperCamelCase(text)
  def lowerCamelCase: String = TextUtils.lowerCamelCase(text)
  def allCaps(sep: String) = TextUtils.allCaps(text, sep)
  def allCaps: String = TextUtils.allCaps(text)
  def smallCaps(sep: String): String = TextUtils.smallCaps(text, sep)
  def smallCaps: String = TextUtils.smallCaps(text)
  def joinText(sep: String): String = TextUtils.joinText(text, sep)
  def joinText: String = TextUtils.joinText(text)
  def indent(prefix: String): String = TextUtils.indent(text, prefix)
  def indent: String = TextUtils.indent(text)
  def indent(prefix: Int): String = TextUtils.indent(text, prefix)
  def exactSplit(separator: Char): List[String] = TextUtils.exactSplit(text, separator)
  def escape: String = TextUtils.escape(text)
}

class RichStringSeq(val list: Seq[String]) extends AnyVal {
  def join(sep: String): String = TextUtils.join(list, sep)
  def join:String = list.mkString
}