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
import org.apache.commons.io.FileUtils._
import eu.lateral.nomic.meno.parsertrans.ParserTranslator
import eu.lateral.nomic.meno.ottranslator.OTTranslator
import eu.lateral.nomic.meno.todefaulttranslator.ToDefaultTranslator
import eu.lateral.nomic.meno.parsertrans.PatternParserTranslator
import eu.lateral.nomic.meno.parsertrans.SimpleParserTranslator
import eu.lateral.nomic.meno.todefaulttrans.ToDefaultTrans
import eu.lateral.nomic.meno.MavenPomGenerator.generatePom
import eu.lateral.nomic.meno.MainClassGenerator.generateMainClass
import eu.lateral.nomic.ObjectTranslators.Translator
//import eu.lateral.nomic.meno.toprologgenerator.ToPrologGenerator
//import eu.lateral.nomic.meno.prologgenerator.PrologGenerator
import eu.lateral.nomic.meno.asttrans.ASTTranslatorAdvancedAbstract
import eu.lateral.nomic.meno.asttrans.ASTTranslatorAdvancedFactory
import eu.lateral.nomic.meno.asttrans.ASTTranslatorAdvancedMain
import eu.lateral.nomic.meno.asttrans.ASTTranslatorAdvancedProxy
import eu.lateral.nomic.meno.asttrans.ASTTranslatorSimple
import eu.lateral.nomic.meno.evaluatortrans._
import java.util.Properties
import java.io.FileReader
import java.io.FileWriter

object Meno extends CommonUtils {
  def message() = println()
  def message(text: String) = println(text)
  def process(pp: ProjectParameters) = {
    message("Meno path:         " + pp.menoPath)
    message("Meno path:         " + pp.menoPath.get)
    message("Properties path:   " + pp.propertiesFile.getAbsolutePath())
    message("Project name:      " + pp.projectName)
    message("Project package:   " + pp.projectPackage)
    message("Project directory: " + pp.projectDir)
    message("Source base:       " + pp.scalaSourcesBaseFile.getPath)
    message("Source directory:  " + pp.scalaSourcesDir)

    trait ProjectProperties {
      self: MenoTranslatorBase =>
      override val properties = pp
    }

    val ast = eu.lateral.nomic.meno.parser.Parser.fromFile(pp.menoPath.get)
    if (!pp.projectFile.mkdirs()) {
      message("Cannot create directory " + pp.projectDir)
    }
    if (!pp.scalaPackageSourcesFile.mkdirs()) {
      message("Cannot create directory " + pp.scalaSourcesDir)
    }

    def createSourceOld(translator: Translator, src: String) = {
      translator.setProperty("package", pp.projectPackage.get)
      val filename = src + ".scala"
      val file: File = new File(pp.scalaSourcesDir.get, filename)
      message("Translating " + src)
      val content = translator.apply(ast)
      if (file.exists()) {
        message("File %s exists => overwriting.".format(filename))
      }
      writeStringToFile(file, content)
      message("Generated %s".format(file.getAbsolutePath))
      message("  (Full path: %s)".format(file.getAbsolutePath))
      message()
    }

    def createSource(translator: MenoTranslatorBase, src: String) = {
      val filename = src + ".scala"
      val file: File = new File(pp.scalaPackageSourcesFile, filename)
      message("Translating " + src)
      val content = translator.apply(ast)
      if (file.exists()) {
        message("File %s exists => overwriting.".format(filename))
      }
      writeStringToFile(file, content)
      message("Generated %s".format(file.getAbsolutePath))
      message("  (Full path: %s)".format(file.getAbsolutePath))
      message()
    }
    
    def createFile(translator: Translator, filename: String) = {
      translator.setProperty("package", pp.projectPackage.get)
      val file = new File(pp.projectDir, filename)
      message("Translating " + filename)
      val content = translator.apply(ast)
      if (file.exists()) {
        message("File %s exists => overwriting.".format(filename))
      }
      writeStringToFile(file, content)
      message("Generated %s".format(file.getAbsolutePath))
      message("  (Full path: %s)".format(file.getAbsolutePath))
      message()
    }

    def generateOnce(dir: String, filename: String, content: => String) {
      val file = new File(dir, filename)
      if (file.exists()) {
        message("File %s exists => kept unchanged.".format(filename))
        message("  (Full path: %s)".format(file.getAbsolutePath))
      } else {
        writeStringToFile(file, content)
        message("Generated %s".format(file.getAbsolutePath))
      }
      message()
    }

    generateOnce(pp.projectDir, "pom.xml", generatePom(pp.mavenGroupId.get, pp.mavenArtefactId.get, pp.projectPackage.get))
    generateOnce(pp.scalaPackageSourcesDir, "Main.scala", generateMainClass(pp))

    if (pp.astAdvanced.get) {
      createSource(new ASTTranslatorAdvancedMain with ProjectProperties, pp.astMainPackage.get)
      createSource(new ASTTranslatorAdvancedAbstract with ProjectProperties, pp.astAbstractPackage.get)
      createSource(new ASTTranslatorAdvancedProxy with ProjectProperties, pp.astProxyPackage.get)
      createSource(new ASTTranslatorAdvancedFactory with ProjectProperties, pp.astFactoryPackage.get)
    } else {
      createSource(new ASTTranslatorSimple with ProjectProperties, pp.astMainPackage.get)
    }
    if (pp.astAdvanced.get) {
      createSource(new SimpleParserTranslator with ProjectProperties, pp.parserPackage.get)
      createSource(new PatternParserTranslator with ProjectProperties, pp.patternParserPackage.get)
    }
    else{
      createSource(new SimpleParserTranslator with ProjectProperties, pp.parserPackage.get)
    }
    createSource(new ToDefaultTrans with ProjectProperties, "defaulttrans")
    if (pp.generateOT.get) {
      createSourceOld(new OTTranslator, "ot")
      createSourceOld(new ToDefaultTranslator, "defaulttranslator")
    }
    if (pp.generateEvaluator.get) {
      createSource(new EvaluatorTrans with ProjectProperties, "evaluator")
    }
    //createSource(new ToPrologGenerator,"prologgenerator")
    //createFile(new PrologGenerator,"metadescription.pl" )
  }

  def main(arg: Array[String]) {
    if (arg.length != 1) {
      println("meno [project.meno]")
    } else {
      process(ProjectParameters.withProperties(arg(0)))
    }
  }
}