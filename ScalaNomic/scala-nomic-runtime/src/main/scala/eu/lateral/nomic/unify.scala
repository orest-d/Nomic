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

package eu.lateral.nomic.unify

object Implicits {

}

trait IContext {
  def getTyped[T: ClassManifest](name: String): Option[T]
  def get(name: String): Option[Unifiable]
}

case class Context(data: Map[String, Unifiable] = Map.empty[String, Unifiable]) extends Map[String, Unifiable] with IContext {
  def get(key: String): Option[Unifiable] = data.get(key)
  def iterator = data.iterator
  def getResolved(key: String): Option[Unifiable] = data.get(key).flatMap {
    case v: Variable => v.get_bound(this)
    case x: Unifiable => Some(x)
  }
  def +[B1 >: Unifiable](kv: (String, B1)) = {
    val kv1: (String, Unifiable) = (kv._1, kv._2.asInstanceOf[Unifiable])
    Context(data + kv1)
  }
  def -(key: String) = Context(data - key)
  def getTyped[T: ClassManifest](name: String): Option[T] = {
    data.get(name).flatMap(x => if (classManifest[T].erasure isAssignableFrom x.getClass) Some(x.asInstanceOf[T]) else None)
  }
  val success = Success(this)
}

trait Unifiable {
  def unify(u: Unifiable, context: Context): Result
  def get_bound(context: Context): Option[Unifiable] = Some(this)
}

trait Variable extends Unifiable {
  def variable_name: String
  override def get_bound(context: Context): Option[Unifiable] = context.get(variable_name).flatMap(_.get_bound(context))

  def unify(u: Unifiable, context: Context) = {
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
        Success(context + (variable_name -> u))
      }
    }
  }
}

case class SimpleVariable(variable_name: String) extends Variable
case class SimpleUnifiable[T](value: T) extends Unifiable {
  def unify(u: Unifiable, context: Context): Result = {
    u match {
      case v: Variable => v.unify(this, context)
      case SimpleUnifiable(x) => if (value == x) context.success else Fail
    }
  }
}

sealed abstract class Result {
  def map(f: Context => Context): Result
  def flatMap(f: Context => Result): Result
  def filter(f: Context => Boolean): Result
  def contexts: Stream[Context]
  def success: Boolean
}

case object Fail extends Result {
  override def map(f: Context => Context) = this
  override def flatMap(f: Context => Result) = this
  def filter(f: Context => Boolean) = this
  def contexts: Stream[Context] = Stream.empty[Context]
  def success = false
}

case class Success(context: Context = new Context) extends Result {
  override def map(f: Context => Context) = Success(f(context))
  override def flatMap(f: Context => Result) = f(context)
  def filter(f: Context => Boolean) = if (f(context)) this else Fail
  def contexts: Stream[Context] = context #:: Stream.empty[Context]
  def success = true
}

class OrResult(val first: Context)(next: => Result) extends Result {
  def second = next
  override def map(f: Context => Context): Result = new OrResult(f(first))(second.map(f))
  override def flatMap(f: Context => Result): Result = {
    f(first) match {
      case Fail => second.flatMap(f)
      case Success(c) => new OrResult(c)(second.flatMap(f))
      case o: OrResult => {
        def n = {
          val c = o.second.contexts #::: second.flatMap(f).contexts
          c.foldRight(Fail: Result)((x, y) => new OrResult(x)(y))
        }
        new OrResult(o.first)(n)
      }
    }
  }
  def filter(f: Context => Boolean) = {
    if (f(first))
      new OrResult(first)(next.filter(f))
    else
      next.filter(f)
  }
  def contexts: Stream[Context] = first #:: second.contexts
  def success = true
}

object Unify {
  def unify(a: Unifiable, b: Unifiable, context: Context): Result = {
    a.get_bound(context).getOrElse(a).unify(b.get_bound(context).getOrElse(b), context)
  }
  def unifyLists(a: List[Unifiable], b: List[Unifiable], context: Context): Result = {
    a match {
      case ahead :: atail => {
        b match {
          case bhead :: btail => {
            for (
              c1 <- ahead.unify(bhead, context);
              c2 <- unifyLists(atail, btail, c1)
            ) yield c2
        }
            case Nil => Fail
          }
      }
      case Nil => {
        b match {
          case Nil => context.success
          case _ => Fail
        }
      }
    }
  }
  
  def unifyOptions(a: Option[Unifiable], b: Option[Unifiable], context: Context): Result = {
    a match{
      case Some(x) => {
          b match{
            case Some(y) => unify(x,y,context)
            case None => Fail
          }
      }
      case None =>{
          b match{
            case None => context.success
            case _ => Fail
          }          
      }
    }
  }

  def unifyBooleans(a: Boolean, b: Boolean, context: Context): Result = {
    if (a==b)
      context.success
    else
      Fail
  }
  
}