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
import eu.lateral.nomic.ASTObjects.ASTObject
import eu.lateral.nomic.errors.PositionError

class MenoTranslatorBase extends CommonUtils{
  import eu.lateral.nomic.TextUtils._
  var statmap: Map[String, Statement] = null

  def apply(obj: Any): String = obj.toString
  val properties = new ProjectParameters
  def getStatementMap(obj: ASTObject): Map[String, Statement] = {
    val smap = for (
      m <- obj.ancestorOrSelf[Main].toList;
      x <- m.sequence.list
    ) yield {
      (for (xx <- x.binaryStatement) yield xx.name.value.lowerCamelCase -> x) ++
        (for (xx <- x.groupStatement) yield xx.name.value.lowerCamelCase -> x) ++
        (for (xx <- x.keywordStatement) yield xx.name.value.lowerCamelCase -> x) ++
        (for (xx <- x.ruleStatement) yield xx.name.value.lowerCamelCase -> x) ++
        (for (xx <- x.tokenStatement) yield xx.name.value.lowerCamelCase -> x)
    }.headOption
    smap.flatten.toMap
  }

  def statementMap(obj: ASTObject): Map[String, Statement] = {
    if (statmap == null) {
      statmap = getStatementMap(obj)
      statmap
    } else {
      statmap
    }
  }
  def ref(name: Identifier) = statementMap(name).get(name.value.lowerCamelCase)
  def refSafe(name: Identifier) = ref(name).getOrElse(throw PositionError(s"Object ${name.value} not found", name.pos))
  def keyword(name: Identifier) = for { x <- ref(name); y <- x.keywordStatement } yield y
  def token(name: Identifier) = for { x <- ref(name); y <- x.tokenStatement } yield y
  def rule(name: Identifier) = for { x <- ref(name); y <- x.ruleStatement } yield y
  def group(name: Identifier) = for { x <- ref(name); y <- x.groupStatement } yield y
  def binary(name: Identifier) = for { x <- ref(name); y <- x.binaryStatement } yield y

  def expandGroup:Boolean = properties.expandGroups.get
  def pkg = properties.projectPackage.get
  def astPackageName = if (properties.astAdvanced.get) properties.astAbstractPackage.get else properties.astMainPackage.get
  def fullASTPackageName = pkg + "." + astPackageName
}