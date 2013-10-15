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
import java.io.File
import eu.lateral.nomic.meno.asttranslator.ASTTranslator
import org.apache.commons.io.FileUtils._
import eu.lateral.nomic.meno.parsertranslator.ParserTranslator
import eu.lateral.nomic.meno.ottranslator.OTTranslator
import eu.lateral.nomic.meno.todefaulttranslator.ToDefaultTranslator
import eu.lateral.nomic.meno.MavenPomGenerator.generatePom
import eu.lateral.nomic.meno.MainClassGenerator.generateMainClass
import eu.lateral.nomic.ObjectTranslators.Translator
import eu.lateral.nomic.meno.toprologgenerator.ToPrologGenerator
import eu.lateral.nomic.meno.prologgenerator.PrologGenerator

object Meno {
  def main(arg: Array[String]) {
    if (arg.length!=1){
      println("meno [project.meno]")
    }
    else{
      val menoPath=arg(0)
      val menoFile = new File(menoPath)
      val nameVector= menoFile.getName.split("[.-]")
      val groupId = nameVector.slice(0,1 max  (nameVector.length-2)).mkString(".")
      val projectName = nameVector.take(nameVector.length-1).mkString("-")
      val projectPackageVector=nameVector.take(nameVector.length-1).map(_.toLowerCase)
      val projectPackage=projectPackageVector.mkString(".")
      val projectFile=new File(menoFile.getParent,projectName)
      val projectDir=projectFile.getAbsolutePath
      val sourceBaseFile=new File(new File(new File(projectDir,"src"),"main"),"scala")
      val sourceFile= projectPackageVector.foldLeft(sourceBaseFile){(total,item)=>new File(total,item)}
      val sourceDir=sourceFile.getAbsolutePath
      println("Meno path:         "+menoPath)
      println("Project name:      "+projectName)
      println("Project package:   "+projectPackage)
      println("Project directory: "+projectDir)
      println("Source base:       "+sourceBaseFile.getPath)
      println("Source directory:  "+sourceDir)
      val ast = eu.lateral.nomic.meno.parser.Parser.fromFile(menoPath)
      if (!projectFile.mkdirs()){
        println("Cannot create directory "+projectDir)
      }
      if (!sourceFile.mkdirs()){
        println("Cannot create directory "+sourceDir)
      }

      def createSource(translator:Translator,src:String) ={
        translator.setProperty("package",projectPackage)
        val filename=src + ".scala"
        val file: File = new File(sourceDir, filename)
        println("Translating "+src)
        val content = translator.apply(ast)
        if (file.exists()){
          println("File %s exists => overwriting.".format(filename))
        }
        writeStringToFile(file, content)
        println("Generated %s".format(file.getAbsolutePath))
        println("  (Full path: %s)".format(file.getAbsolutePath))
        println()
      }

      def createFile(translator:Translator,filename:String) ={
        translator.setProperty("package",projectPackage)
        val file=new File(projectDir,filename)
        println("Translating "+filename)
        val content = translator.apply(ast)
        if (file.exists()){
          println("File %s exists => overwriting.".format(filename))
        }
        writeStringToFile(file, content)
        println("Generated %s".format(file.getAbsolutePath))
        println("  (Full path: %s)".format(file.getAbsolutePath))
        println()
      }

      def generateOnce(dir:String,filename:String,content: =>String){
        val file = new File(dir,filename)
        if (file.exists()){
          println("File %s exists => kept unchanged.".format(filename))
          println("  (Full path: %s)".format(file.getAbsolutePath))
        }
        else{
          writeStringToFile(file,content)
          println("Generated %s".format(file.getAbsolutePath))
        }
        println()
      }

      println()
      generateOnce(projectDir,"pom.xml",generatePom(groupId,projectName,projectPackage))
      generateOnce(sourceDir,"Main.scala",generateMainClass(projectPackage))

      createSource(new ASTTranslator,"ast")
      createSource(new ParserTranslator,"parser")
      createSource(new OTTranslator,"ot")
      createSource(new ToDefaultTranslator,"defaulttranslator")
      createSource(new ToPrologGenerator,"prologgenerator")
      createFile(new PrologGenerator,"metadescription.pl" )
    }
  }
}
