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

package eu.lateral.nomic.errors

import scala.util.parsing.input.Position

abstract class AbstractError{
  def errorMessage:String
}

case class SimpleError(message:String) extends AbstractError{
  override def errorMessage = "ERROR: "+message
}

case class PositionError(message:String,pos:Position) extends AbstractError{
  override def errorMessage = "ERROR: " + message +" "+pos
}
