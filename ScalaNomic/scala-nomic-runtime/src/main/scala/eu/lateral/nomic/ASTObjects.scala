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

package eu.lateral.nomic.ASTObjects
import scala.util.parsing.input.Positional

class ASTObject extends Positional{
  var parent:Option[ASTObject] = None
  def ancestor[T : ClassManifest]():Option[T] =  parent match {  
    case Some(x) =>  if (x.getClass()==classManifest[T].erasure) Some(x.asInstanceOf[T]) else x.ancestor[T]         
    case None => None
  }
}

class Literal(val literal:String) extends ASTObject{
  override def toString = "Literal(%s)".format(literal)
}

class AGroup(val groupContent:ASTObject) extends ASTObject{
  groupContent.parent = Some(this)
  override def toString = "AGroup(%s)".format(groupContent.toString)
}

case class AList[T<:ASTObject](val list:List[T]) extends ASTObject{
  list.foreach(_.parent=Option(this))
  override def toString = list.toString
}
