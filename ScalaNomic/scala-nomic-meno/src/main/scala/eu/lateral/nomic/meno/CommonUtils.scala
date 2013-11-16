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

trait CommonUtils extends {
  import eu.lateral.nomic.TextUtils._

  implicit def toExtString(text: String) = new ExtString(text)
  implicit def stringToBoolean(text: String): Boolean = List("true", "yes", "y", "t").contains(text.toLowerCase())
}

class ExtString(val text: String) extends AnyVal {
  import eu.lateral.nomic.TextUtils._
  def cname = astname + "T"
  def refname = {
    val s = text.lowerCamelCase
    if (List("literal", "regex").contains(s)) "o_" + s else s
  }
  def astname = {
    val s = text.upperCamelCase
    if (List("Regex", "String", "Option").contains(s)) "AST" + s else s
  }
  def traitname = astname + "Trait"
  def evalname = astname + "E"
}