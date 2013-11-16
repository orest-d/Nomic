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

import java.util.Properties

object MainClassGenerator extends CommonUtils{
  def generateMainClass(properties: Properties) = {
    val pkg = properties.getProperty("package")
    val evaluator = if (properties.getProperty("generate.evaluator","no")){
      s"      println\n      $pkg.evaluator.Evaluator.main(arg)\n      println\n"
    }
    else{
      ""
    }
    s"""
    |package $pkg
    |import java.io.File
    |import $pkg.defaulttrans.DefaultTranslator
    |import $pkg.parser.Parser
    |import org.apache.commons.io.FileUtils.writeStringToFile
    |import eu.lateral.nomic.ObjectTranslators.Translator
    |
    |object Main {
    |  def main(arg: Array[String]) {
    |    if (arg.length<1){
    |      println("main [input [output]]")
    |    }
    |    else{
    |      val path=arg(0)
    |      val ast = Parser.fromFile(path)
    |      println(ast)
    |      println
    |      def output(translator:Translator)={
    |        translator.setProperty("input",path)
    |        translator(ast)
    |      }
    |      def outputToFile(translator:Translator,path:String){
    |        writeStringToFile(new File(path), output(translator))
    |      }
    |      val translator = new DefaultTranslator
    |
    |      if (arg.length>=2){
    |        outputToFile(translator,arg(1))
    |      }
    |      else{
    |        println(output(translator))
    |      }
    |
    |$evaluator
    |    }
    |  }
    |}
  """.stripMargin
  }
}
