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
  def getUnifiable(name: String): Option[Unifiable]
}

case class Context(data: Map[String, Any] = Map.empty[String, Any]) extends Map[String, Any] with IContext {
  def get(key: String) = data.get(key)
  def iterator = data.iterator
  def + [B1 >: Any](kv: (String, B1)) = Context(data + kv)
  def -(key: String) = Context(data - key)
  def getTyped[T: ClassManifest](name: String): Option[T] = {
    data.get(name).flatMap(x => if (classManifest[T].erasure isAssignableFrom x.getClass) Some(x.asInstanceOf[T]) else None)
  }
  def getUnifiable(name: String): Option[Unifiable] = {
    data.get(name).flatMap(x => if (classOf[Unifiable] isAssignableFrom x.getClass) Some(x.asInstanceOf[Unifiable]) else None)
  }
}

trait Unifiable {
  def unify(u: Unifiable, c: Context): Result
  def get_bound(c: Context): Option[Unifiable] = Some(this)
  //  def get_leaf(c:Context):Unifiable = this
}

trait Variable extends Unifiable {
  def variable_name: String
  override def get_bound(c: Context): Option[Unifiable] = c.getUnifiable(variable_name).flatMap(_.get_bound(c))

  //override def get_leaf(c:Context):Unifiable = c.getUnifiable(variable_name).map(_.get_leaf(c)).getOrElse(this)

  def unify(u: Unifiable, c: Context) = {

    val me = get_bound(c)
    val other = u.get_bound(c)
    if (me.isDefined) {
      if (other.isDefined) {
        me.get.unify(other.get, c)
      } else {
        u.unify(me.get, c)
      }
    } else {
      Success(c + (variable_name -> u))
    }

  }

}

case class SimpleVariable(variable_name:String) extends Variable
case class SimpleUnifiable[T](value:T) extends Unifiable{
  def unify(u: Unifiable, c: Context): Result = u match{
    case SimpleUnifiable(x) => if (u == x) SimpleSuccess else Fail
    case v:Variable => v.unify(u,c)
  }  
}

sealed abstract class Result {
  def map(f: Context => Context): Result
  def flatMap(f: Context => Result): Result
  def status: String
  def contexts: Stream[Context]
}

case object Fail extends Result {
  override def map(f: Context => Context) = this
  override def flatMap(f: Context => Result) = this
  def status: String = "Fail"
  def contexts: Stream[Context] = Stream.empty[Context]
}

case class UnificationError(exception: Throwable) extends Result {
  override def map(f: Context => Context) = this
  override def flatMap(f: Context => Result) = this
  def status: String = "ERROR: " + exception.getMessage
  def contexts: Stream[Context] = Stream.empty[Context]
}

case class Success(context: Context = new Context) extends Result {
  override def map(f: Context => Context) = Success(f(context))
  override def flatMap(f: Context => Result) = f(context)
  def status: String = "Success"
  def contexts: Stream[Context] = context #:: Stream.empty[Context]
}

object SimpleSuccess extends Success

class OrResult(val first: Context)(next: => Result) extends Result {
  def second = next
  override def map(f: Context => Context): Result = new OrResult(f(first))(second.map(f))
  override def flatMap(f: Context => Result): Result = {
    f(first) match {
      case Fail => second.flatMap(f)
      case err: UnificationError => err
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
  def status: String = "Success"
  def contexts: Stream[Context] = first #:: second.contexts
}

