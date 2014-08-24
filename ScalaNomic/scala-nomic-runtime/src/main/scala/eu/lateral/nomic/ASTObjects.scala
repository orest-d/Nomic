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
import eu.lateral.nomic.unify._
import scala.util.parsing.input.Positional

class ASTObject extends Positional with Unifiable {
  var parent: Option[ASTObject] = None
  def ancestor[T: ClassManifest](): Option[T] = parent match {
    case Some(x) => if (x.getClass() == classManifest[T].erasure) Some(x.asInstanceOf[T]) else x.ancestor[T]
    case None => None
  }
  def ancestorOrSelf[T: ClassManifest](): Option[T] = {
    if (this.getClass() == classManifest[T].erasure) {
      Some(this.asInstanceOf[T])
    } else {
      ancestor[T]
    }
  }
  def is_equal(other_o: Any) = this == other_o
  def error_message: String = null
  def get_children: Stream[ASTObject] = Stream.empty[ASTObject]
  def get_topDown: Stream[ASTObject] = this #:: get_children.flatMap(_.get_topDown)
  def get_bottomUp: Stream[ASTObject] = get_children.flatMap(_.get_bottomUp) #::: this #:: Stream.empty

  def unify(u: Unifiable, context: Context): Result = {
    u match {
      case v: Variable => {
        v.unify(this, context)
      }
      case x: ASTObject => {
        if (this is_equal x)
          context.success
        else
          Fail
      }
    }
  }
}

abstract class AbstractLiteral extends ASTObject {
  def value = literal
  def literal: String
  override def toString = "AbstractLiteral"  
}

class Literal(val literal: String) extends AbstractLiteral {
  override def toString = s"Literal($literal)"
}

case class ParsingError(message: String) extends ASTObject {
  override def error_message = message
}

abstract class AbstractGroup extends ASTObject {
  def content: ASTObject
  def set_parent = { content.parent = Some(this) }
  override def toString = "AGroup(%s)".format(content.toString)
  override def get_children = content #:: Stream.empty[ASTObject]
}

class AGroup(val content: ASTObject) extends AbstractGroup {
  set_parent
}

abstract class AbstractList[T <: ASTObject] extends ASTObject {
  def list: List[T]
  def set_parent = {
    list.foreach(_.parent = Option(this))
  }
  override def toString = list.toString
  override def get_children = list.toStream #::: Stream.empty[ASTObject]
  override def is_equal(other_o: Any) = other_o match {
    case o: AList[T] =>
      (list.length == o.list.length) && get_children.zip(o.get_children).forall(x => x._1 is_equal x._2)
    case _ => false
  }

  override def unify(u: Unifiable, context: Context): Result = u match {
    case x: AbstractList[T] => Unify.unifyLists(list, x.list, context)
    case _ => Fail
  }
}
case class AList[T <: ASTObject](list: List[T]) extends AbstractList[T] {
  set_parent
}

abstract class AbstractBinary[A <: ASTObject] extends ASTObject {
  def left: A
  def right: A
  def set_parent = {
    left.parent = Option(this)
    right.parent = Option(this)
  }
  override def toString = s"Binary($left, $right)"
  override def get_children = left #:: right #:: Stream.empty[ASTObject]
  override def unify(u: Unifiable, context: Context): Result = u match {
    case v: Variable => v.unify(this, context)    
    case x: AbstractBinary[A] => {       
      for (
        c1 <- left.unify(x.left, context);
        c2 <- right.unify(x.right, c1)
      ) yield c2
    }
    case _ => Fail
}
  }

class ABinary[A <: ASTObject](val left: A, val right: A) extends AbstractBinary[A]

trait ASTVariable extends Variable {
  def proxy_class: Class[_]

  override def unify(u: Unifiable, context: Context): Result = {
    if (u == this) {
      context.success
    } else {
      val me = get_bound(context)
      if (me.isDefined) {
        val other = u.get_bound(context)
        if (other.isDefined) {
          me.get.unify(other.get, context)
        } else {
          u.unify(me.get, context)
        }
      } else {
        if (proxy_class isAssignableFrom u.getClass) {
          Success(context + (variable_name -> u))
        } else Fail
      }
    }
  }
}
