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
import scala.collection.mutable.Map
import java.util.Properties
import java.io.File
import java.io.FileReader
import java.io.FileWriter
import eu.lateral.nomic.TextUtils._

case class ParameterParsingError(value: String, message: String) extends Throwable(s"$message (value = $value)")
case class PropertyParsingError(key: String, value: String, message: String) extends Throwable(s"$message (key = <$key>, value = <$value>)")
case class ParameterNotSpecified extends Throwable("Parameter not specified")
case class PropertyNotSpecified(key: String) extends Throwable(s"Property $key not specified")

abstract class Parameter {
  def getString: String
  def setString(newValue: String)
}

abstract class TypedParameter[T](default: () => T) extends Parameter {
  var value: Option[T] = None
  def getDefaultValue = default
  def get: T = value.getOrElse(default())
  def set(newValue: T) { value = Some(newValue) }
  override def getString: String = { get.toString }
  override def toString():String = getString
}

class StringParameter(default: () => String = () => "") extends TypedParameter[String](default) {
  override def get = value.map(_.trim).getOrElse(default())
  def setString(newValue: String) { value = Some(newValue) }
}

class YesNoParameter(default: () => Boolean) extends TypedParameter[Boolean](default) {
  override def getString: String = if (get) "yes" else "no"
  def setString(newValue: String) {
    val s = newValue.trim.toLowerCase()
    if (s == "yes") {
      value = Some(true)
    } else if (s == "no") {
      value = Some(false)
    } else {
      throw ParameterParsingError(newValue, "YES or NO expected")
    }
  }
}

case class ParameterRegistration(parameter: Parameter, key: String, comment: String)

class EmptyProjectParameters {
  var registry = List.empty[ParameterRegistration]
  def register(key: String, default: ()=>String, comment: String): StringParameter = {
    val param = new StringParameter(default)
    registry = ParameterRegistration(param, key, comment) :: registry
    param
  }
  def register(key: String, default: ()=>Boolean, comment: String): YesNoParameter = {
    val param = new YesNoParameter(default)
    registry = ParameterRegistration(param, key, comment) :: registry
    param
  }
  def register(key: String, default: String, comment: String): StringParameter = register(key,()=>default,comment)
  def register(key: String, default: Boolean, comment: String): YesNoParameter = register(key,()=>default,comment)
  def properties = {
    val p = for (reg <- registry.reverse) yield {
      val comment = if (reg.comment.length() == 0) "" else "\n# " + reg.comment +"\n"
      "%s%-20s = %-20s\n".format(comment,reg.key, reg.parameter.getString)
    }
    p.mkString
  }
  def mavenProperties = {
    val p = for (reg <- registry.reverse) yield {
      val comment = if (reg.comment.length() == 0) "" else "<!-- " + reg.comment + " -->\n"
      val tag = reg.key.lowerCamelCase
      s"$comment<$tag>${reg.parameter.getString}</$tag>\n"
    }
    p.mkString
    
  }
  def updateFrom(prop: Properties) {
    for (reg <- registry) {
      val p = prop.getProperty(reg.key)
      if (p != null) {
        try {
          reg.parameter.setString(p)
        } catch {
          case ParameterParsingError(value: String, message: String) => throw PropertyParsingError(reg.key, value, message)
        }
      }
    }
  }
}

class ProjectParameters extends EmptyProjectParameters {
  import ProjectParameters.valueFromParameter
  val menoPath = register("meno.path", "dsl.meno", "Path to the .meno file")
  val projectName = register("project.name", ()=>nameVector.take(nameVector.length - 1).mkString("-"), "Name of the project")
  def menoFile = new File(menoPath)
  def nameVector = menoFile.getName.split("[.-]")

  val propertiesName = register("properties.name",()=>nameVector.take(nameVector.length - 1).mkString("-") + ".properties","")
  val propertiesPath = register("properties.path",()=>{
    val f = new File(menoFile.getParent, propertiesName)
    f.getAbsolutePath()
  },"")
  def propertiesFile = new File(propertiesPath)

  val mavenGroupId = register("maven.groupId", () => nameVector.slice(0, 1 max (nameVector.length - 2)).mkString("."), "Maven groupId for the project")
  val mavenArtefactId = register("maven.artefactId", () => projectName.get, "Maven artefactId for the project")
  val projectPackage = register("project.package", ()=>nameVector.take(nameVector.length - 1).map(_.toLowerCase).mkString("."), "Main project package")
  def projectPackageVector = projectPackage.get.split("[.]")

  def projectFile = new File(menoFile.getParent, projectName)
  def projectDir = projectFile.getPath
  def scalaSourcesDir = register("scala.sources", ()=>(new File(new File(new File(projectDir, "src"), "main"), "scala")).getPath, "Path to the scala sources")
  def scalaSourcesBaseFile = new File(scalaSourcesDir)
  def scalaPackageSourcesFile = projectPackageVector.foldLeft(scalaSourcesBaseFile) { (total, item) => new File(total, item) }
  def scalaPackageSourcesDir = scalaPackageSourcesFile.getAbsolutePath() 

  val generateEvaluator=register("generate.evaluator", true, "decides if evaluator will be generated")
  val generateOT=register("generate.ot", false, "this is an obsolete functionality")
  val expandGroups=register("groups.expand",true,"if true, groups withing groups will be replaced by its members")
  val astAdvanced=register("ast.advanced",true,"if true, generates abstract ast node, proxy, is_equal and other")
  val astMainPackage=register("ast.main.package","ast","package name for AST nodes (value classes)")
  val astAbstractPackage=register("ast.abstract.package",()=>if (astAdvanced.get) "aast" else "ast","package name for abstract AST nodes (only applies if ast.advanced is true)")
  val astProxyPackage=register("ast.proxy.package",()=>if (astAdvanced.get) "astproxy" else "ast","package name for proxy AST nodes (only applies if ast.advanced is true)")
  val astFactoryPackage=register("ast.factory.package",()=>if (astAdvanced.get) "astfactory" else "ast","package name for AST factory/singletons (only applies if ast.advanced is true)")
  val parserPackage=register("parser.package","parser","parser package name")
  val patternParserPackage=register("patternparser.package","patternparser","pattern parser package name (only applies if ast.advanced is true)")
  val patternParserVariablePrefix=register("patternparser.variable.prefix","$","beginning of the variable pattern")
  val patternParserVariablePostfix=register("patternparser.variable.postfix","","end of the variable pattern")
  val patternParserVariableTypeSeperator=register("patternparser.variable.typeseparator",":","string separating the variable name from its type in pattern parser variables")
}

object ProjectParameters{
  implicit def valueFromParameter[T](p: TypedParameter[T]):T = p.get
  def apply(menoPath:String)={
    val p = new ProjectParameters
    p.menoPath.setString(menoPath)
    p
  }
  def withProperties(menoPath:String)={
    import org.apache.commons.io.FileUtils._
    
    val p = new ProjectParameters
    p.menoPath.set(menoPath)
    println("menoPath:"+menoPath)
    println(p.properties)
    if (p.propertiesFile.exists()) {
      val properties = new Properties
      val f = new FileReader(p.propertiesFile)
      properties.load(f)
      f.close
      p.updateFrom(properties)
    }
    else{
      writeStringToFile(p.propertiesFile, "# Autogenerated meno project file\n"+p.properties)
    }
    p
  }
}