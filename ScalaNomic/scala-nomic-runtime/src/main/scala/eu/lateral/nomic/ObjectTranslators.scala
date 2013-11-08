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

package eu.lateral.nomic.ObjectTranslators
import eu.lateral.nomic.ASTObjects._
import eu.lateral.nomic.TextUtils
import eu.lateral.nomic.errors._
import scala.util.parsing.input.Position

class Translator {
  def apply(obj: Any) = obj.toString
  def error(err:AbstractError)       {println(err.errorMessage)}
  def error(err:String)              {error(SimpleError(err))}
  def property(name:String)=""
  def setProperty(name:String,value:String){}
}

class TranslatorWithProperties extends Translator{
  var properties:Map[String,String] = Map()
  override def property(name: String) = {
    if (properties.contains(name)){
      properties(name)
    }
    else{
      error("Property '%s' not known.".format(name))
      ""
    }
  }

  override def setProperty(name: String, value: String) {
    properties+=(name->value)
  }
}

abstract class OT[In, Out] {
  implicit def toStringOT[A](ot:OT[A,String]) = new StringOT[A](ot)
  implicit def toBooleanOT[A](ot:OT[A,Boolean]) = new BooleanOT[A](ot)
  implicit def toOptionOT[A,B](ot:OT[A,Option[B]]) = new OptionOT[A,B](ot)
  implicit def toToListOT[A,B](ot:OT[A,List[B]]) = new ListOT[A,B](ot)
  implicit def toAnyOT[A,B](ot:OT[A,B]) = ot.A

  def apply(translator: Translator, obj: In): Out
  def /[Next](next: OT[Out, Next]) = new ComposeOT(this, next)
  def /[Next](f: (Out => Next)): ComposeOT[In, Out, Next] = this / new FunctionOT(f)
  def /[Next](f: (Translator, Out) => Next): ComposeOT[In, Out, Next] = this / new TranslatorFunctionOT(f)
  def /(next: OT[Out, String]) = new StringOT(new ComposeOT(this, next))
  //def /(f: (Out => String)):StringOT[In] = new StringOT(this / new FunctionOT(f))
  //def /(f: (Translator, Out) => String):StringOT[In] = new StringOT(this / new TranslatorFunctionOT(f))

  def F[Next](f: (In => Next)) = new FunctionOT(f)
  def F[Next](f: (Translator, In) => Next)= new TranslatorFunctionOT(f)
  //def F(f: (In => String)) = new StringOT(new FunctionOT(f))
  //def F(f: (Translator, In) => String)= new StringOT(new TranslatorFunctionOT(f))

  def === (x:OT[In,Out]) = new BooleanOT[In](F{
    (translator:Translator,obj:In) => this(translator,obj) == x(translator,obj)
  })
  def T = new StringOT(new ComposeOT(this, new TranslateOT()))
  def C[Next](value: Next) = new ConstantOT[In, Next](value)
  def C(value: String) = new StringOT(new ConstantOT[In, String](value))
  def A = (this / (_.asInstanceOf[Any])).asInstanceOf[OT[In,Any]]
  def str = new StringOT(this / (_.toString()))
  def format(formatrstring:String,arg:OT[In, Any]*) = new FormatOT(formatrstring,arg:_*)
  def translatorProperty(name:String) = new StringOT(F((translator,obj) => translator.property(name)))
}

class IdentityOT[T] extends OT[T, T] {
  override def apply(translator: Translator, obj: T): T = obj
}

abstract class AbstractStringOT[T] extends OT[T,String]{
  def toLowerCase = new StringOT(this / (_.toLowerCase))
  def toUpperCase = new StringOT(this / (_.toUpperCase))
  def toLowerCamelCase = new StringOT(this / (TextUtils.lowerCamelCase(_)))
  def toUpperCamelCase = new StringOT(this / (TextUtils.upperCamelCase(_)))
  def toAllCaps = new StringOT(this / (TextUtils.allCaps(_)))
  def toSmallCaps = new StringOT(this / (TextUtils.smallCaps(_)))
  def escape: StringOT[T] = new StringOT(this / (TextUtils.escape(_)))
  def indent: StringOT[T] = new StringOT(this / (TextUtils.indent(_)))
  def indent(n: Int): StringOT[T] = new StringOT(this / (x => { TextUtils.indent(x, n) }))
  def indent(sep: String): StringOT[T] = new StringOT(this / (x => { TextUtils.indent(x, sep) }))
  def +(next: OT[T, String]) = new StringOT(new StringPlusOT(this, next))  
}

class StringOT[T](first: OT[T, String]) extends AbstractStringOT[T]{
  override def apply(translator: Translator, obj: T): String = first(translator, obj)  
} 

object StringOT extends StringOT(new IdentityOT[String])

class StringPlusOT[T](first: OT[T, String], second: OT[T, String]) extends OT[T, String] {
  override def apply(translator: Translator, obj: T): String = first(translator, obj) + second(translator, obj)
}

class TranslateOT[T] extends OT[T, String] {
  override def apply(translator: Translator, obj: T): String = translator(obj)
}

class ComposeOT[A, B, C](first: OT[A, B], second: OT[B, C]) extends OT[A, C] {
  override def apply(translator: Translator, obj: A): C = second(translator, first(translator, obj))
}

class GetOrElseOT[In,Out](optionOt:OT[In, Option[Out]],elseOt:OT[In,Out]) extends OT[In, Out] {
  def apply(translator: Translator, obj: In) = {
    optionOt(translator,obj).getOrElse(elseOt(translator,obj))
  }
}
class IfDefinedOrElseOT[In,T,Out](optionOt:OT[In, Option[T]],ifDefinedOt:OT[In,Out],elseOt:OT[In,Out]) extends OT[In, Out] {
  def apply(translator: Translator, obj: In) = {
    if (optionOt(translator,obj).isDefined)
      ifDefinedOt(translator,obj)
    else
      elseOt(translator,obj)
  }
}

class StringIfDefinedOrElseOT[In,T](optionOt:OT[In, Option[T]],ifDefinedOt:OT[In,String],elseOt:OT[In,String]) extends AbstractStringOT[In] {
  def apply(translator: Translator, obj: In) = {
    if (optionOt(translator,obj).isDefined)
      ifDefinedOt(translator,obj)
    else
      elseOt(translator,obj)
  }
}

class TranslateOrElseOT[In,Out](optionOt:OT[In, Option[Out]],elseOt:OT[In,String]) extends AbstractStringOT[In] {
  def apply(translator: Translator, obj: In) = {
    optionOt(translator,obj).map(translator(_)).getOrElse(elseOt(translator,obj))
  }
}

abstract class AbstractOptionOT[In, Out] extends OT[In, Option[Out]] {
  def getOrElse(x: Out) = this / (_.getOrElse(x))
  def getOrElse(x:OT[In,Out]) = new GetOrElseOT[In,Out](this,x)
  def isDefined = new BooleanOT[In](this/(_.isDefined))
  def ifDefinedOrElse[T](ifDefinedOt:OT[In,T],elseOt:OT[In,T])=new IfDefinedOrElseOT[In,Out,T](this,ifDefinedOt,elseOt)
  def ifDefinedOrElse(ifDefinedOt:OT[In,String],elseOt:OT[In,String])=new StringIfDefinedOrElseOT[In,Out](this,ifDefinedOt,elseOt)
  def translateOrElse(x: String) = new StringOT[In](
    this / ((t:Translator,obj:Option[Out]) => obj.map(t(_)).getOrElse(x)))
  def translateOrElse(x:OT[In,String]) = new TranslateOrElseOT[In,Out](this,x)
  def ?[Next](next: OT[Out, Next]) = new OptionComposeOT(this, next)
  def str(default: String) = new StringOT(
    this / {
      _ match {
        case Some(x) => x.toString
        case None => default
      }
    })
  def T(default: String) = new StringOT(
    this / {
      (translator:Translator,o:Option[Out]) =>o match {
        case Some(x) => translator(x)
        case None => default
      }
    })
}
class ChainingOptionOT[In,Out,T](first: OT[In, Option[Out]],chainCreator:OT[In,Out]=>T) extends AbstractOptionOT[In, Out]{
  def get = chainCreator(this/(_.get))
  def getOrFail(msg:OT[In,String])=chainCreator(F{
    (translator,obj)=>{
      val option = first(translator,obj)
      if (option.isDefined)
        option.get
      else{
        translator.error(msg(translator,obj))
        option.get
      }
    }
  })
  def getOrFail(msg:OT[In,String],pos:OT[In,Position])=chainCreator(F{
    (translator,obj)=>{
      val option = first(translator,obj)
      if (option.isDefined)
        option.get
      else{
        translator.error(PositionError(msg(translator,obj),pos(translator,obj)))
        option.get
      }
    }
  })
  override def apply(translator: Translator, obj: In) = first(translator, obj)
}
class OptionOT[In, Out](first: OT[In, Option[Out]]) extends AbstractOptionOT[In, Out] {
  def get = this/(_.get)
  override def apply(translator: Translator, obj: In) = first(translator, obj)
}

class IdentityOptionOT[T] extends OptionOT(new IdentityOT[Option[T]])

class OptionComposeOT[A, B, C](first: OT[A, Option[B]], second: OT[B, C]) extends AbstractOptionOT[A, C] {
  def get = this/(_.get)
  override def apply(translator: Translator, obj: A): Option[C] = {
    first(translator, obj) match {
      case Some(x) => Some(second(translator, x))
      case None => None
    }
  }
}

class FunctionOT[A, B](f: (A => B)) extends OT[A, B] {
  override def apply(translator: Translator, obj: A): B = f(obj)
}

class TranslatorFunctionOT[A, B](f: ((Translator, A) => B)) extends OT[A, B] {
  override def apply(translator: Translator, obj: A): B = f(translator, obj)
}

class ConstantOT[X, T](c: T) extends OT[X, T] {
  override def apply(translator: Translator, obj: X): T = c
}

class ProxyOT[A, B](first: OT[A, B]) extends OT[A, B] {
  override def apply(translator: Translator, obj: A): B = first(translator, obj)
}

class ASTObjectOT[In, Out <: ASTObject](first: OT[In, Out]) extends OT[In, Out] {
  override def apply(translator: Translator, obj: In) = first(translator, obj)
  def ancestor[T:ClassManifest] = new OptionOT(this/((x:Out) => x.ancestor[T]) )
  def pos = this / ((_:Out).pos)
}

class LiteralOT[T, L <: Literal](first: OT[T, L]) extends ASTObjectOT[T, L](first) {
  def value: StringOT[T] = new StringOT(first / (_.literal))
}

class BooleanOT[T](first: OT[T, Boolean]) extends OT[T, Boolean] {
  override def apply(translator: Translator, obj: T) = first(translator, obj)
  def ifTrueElse[Next](ontrue:Next,onfalse:Next)=this/{if(_) ontrue else onfalse}
  def ifTrueElse[Next](ontrue:OT[T,Next],onfalse:OT[T,Next])=F{
    (translator,obj) =>{
      if (this(translator,obj))
        ontrue(translator,obj)
      else
        onfalse(translator,obj)
    }
  }
  def ifTrueElse(ontrue:String,onfalse:String)=new StringOT(this/{if(_) ontrue else onfalse})
  def unary_! = new BooleanOT[T](this/(!(_)))
  def &&(x:OT[T,Boolean])=new BooleanOT[T](F {
    (translator,obj)=>{this(translator,obj) && x(translator,obj)}
  })
  def ||(x:OT[T,Boolean])=new BooleanOT[T](F {
    (translator,obj)=>{this(translator,obj) || x(translator,obj)}
  })
}
object BooleanOT extends BooleanOT(new IdentityOT[Boolean])

class JoinListOT[T,E](list: OT[T, List[E]],splitBy:OT[T,String]) extends AbstractStringOT[T]{
  def apply(translator: Translator, obj: T) = {
    list(translator,obj).map(translator(_)).mkString(splitBy(translator,obj))
  }
}

class ListOT[T, E](first: OT[T, List[E]]) extends OT[T, List[E]] {
  override def apply(translator: Translator, obj: T) = first(translator, obj)
  def join(splitBy: String): StringOT[T] = new StringOT(this / ((translator:Translator,x: List[E]) => {
    def joinTwo(a:String,b:String) = {
      if (a.length==0) b else if (b.length==0) a else a+splitBy+b
    }
    val l = x.map(translator(_))
    if (l.length==0) ""
    else if (l.length ==1) l(0)
    else l.reduceLeft(joinTwo)
    }))
  def join(splitBy: OT[T,String]) = new JoinListOT[T,E](this,splitBy)

  def join: StringOT[T] = join("")

  def map[F](f:OT[E,F]):ListOT[T,F] = new ListOT(this/{
    (translator:Translator,obj:List[E])=>{
      for(x<-obj) yield f(translator,x)
    }
  })
  def map[F](f:(E=>F)):ListOT[T,F] = new ListOT(this/{
    (translator:Translator,obj:List[E])=>{
      for(x<-obj) yield f(x)
    }
  })

  def filter(f:OT[E,Boolean]):ListOT[T,E] = new ListOT(this/{
    (translator:Translator,obj:List[E])=>{
      obj.filter(f(translator,_)) 
    }
  })
  def filter(f:(E=>Boolean)):ListOT[T,E] = new ListOT(this/{
    (obj:List[E])=>{obj.filter(x => f(x))}
  })
}

class IdentityListOT[T] extends ListOT(new IdentityOT[List[T]])

class BinaryOT[
  In,
  Operator <: ABinary[Operand],
  Operand <: ASTObject,
  OperandOT ] (first:OT[In,Operator],factory:(OT[In,Operand] => OperandOT)) extends ASTObjectOT(first){
  val left  = factory(this/((_:Operator).left))
  val right = factory(this/((_:Operator).right))
} 

class FormatOT[T](format:String,arg:OT[T,Any]*) extends AbstractStringOT[T]{
  override def apply(translator:Translator,obj:T) = format.format(
      (for(f<-arg) yield f(translator,obj)):_*
  )
}
