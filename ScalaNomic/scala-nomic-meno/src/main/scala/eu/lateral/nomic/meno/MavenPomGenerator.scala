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

/**
 * Created with IntelliJ IDEA.
 * User: orest
 * Date: 9/24/12
 * Time: 10:03 PM
 * To change this template use File | Settings | File Templates.
 */
object MavenPomGenerator {

  def generatePom(groupId:String,artifactId:String,packagename:String)=
    """
      |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      |  <modelVersion>4.0.0</modelVersion>
      |  <groupId>%s</groupId>
      |  <artifactId>%s</artifactId>
      |  <version>1.0-SNAPSHOT</version>
      |  <name>${project.artifactId}</name>
      |  <description>My wonderfull meno-DSL</description>
      |  <inceptionYear>2010</inceptionYear>
      |  <licenses>
      |    <license>
      |      <name>My License</name>
      |      <url>http://....</url>
      |      <distribution>repo</distribution>
      |    </license>
      |  </licenses>
      |
      |  <properties>
      |    <maven.compiler.source>1.5</maven.compiler.source>
      |    <maven.compiler.target>1.5</maven.compiler.target>
      |    <encoding>UTF-8</encoding>
      |    <scala.version>2.8.0</scala.version>
      |  </properties>
      |
      |<!--
      |  <repositories>
      |    <repository>
      |      <id>scala-tools.org</id>
      |      <name>Scala-Tools Maven2 Repository</name>
      |      <url>http://scala-tools.org/repo-releases</url>
      |    </repository>
      |  </repositories>
      |
      |  <pluginRepositories>
      |    <pluginRepository>
      |      <id>scala-tools.org</id>
      |      <name>Scala-Tools Maven2 Repository</name>
      |      <url>http://scala-tools.org/repo-releases</url>
      |    </pluginRepository>
      |  </pluginRepositories>
      |-->
      |  <dependencies>
      |    <dependency>
      |      <groupId>org.scala-lang</groupId>
      |      <artifactId>scala-library</artifactId>
      |      <version>${scala.version}</version>
      |    </dependency>
      |    <dependency>
      |      <groupId>eu.lateral</groupId>
      |      <artifactId>scala-nomic-runtime</artifactId>
      |      <version>0.0.1-SNAPSHOT</version>
      |    </dependency>
      |    <dependency>
      |      <groupId>commons-io</groupId>
      |      <artifactId>commons-io</artifactId>
      |      <version>1.4</version>
      |    </dependency>
      |
      |    <!-- Test -->
      |    <dependency>
      |      <groupId>junit</groupId>
      |      <artifactId>junit</artifactId>
      |      <version>4.8.1</version>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>org.scala-tools.testing</groupId>
      |      <artifactId>specs_${scala.version}</artifactId>
      |      <version>1.6.5</version>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>org.scalatest</groupId>
      |      <artifactId>scalatest</artifactId>
      |      <version>1.2</version>
      |      <scope>test</scope>
      |    </dependency>
      |  </dependencies>
      |
      |  <build>
      |    <sourceDirectory>src/main/scala</sourceDirectory>
      |    <testSourceDirectory>src/test/scala</testSourceDirectory>
      |    <plugins>
      |      <plugin>
      |        <groupId>org.scala-tools</groupId>
      |        <artifactId>maven-scala-plugin</artifactId>
      |        <version>2.15.0</version>
      |        <executions>
      |          <execution>
      |            <goals>
      |              <goal>compile</goal>
      |              <goal>testCompile</goal>
      |            </goals>
      |            <configuration>
      |              <args>
      |                <arg>-make:transitive</arg>
      |                <arg>-dependencyfile</arg>
      |                <arg>${project.build.directory}/.scala_dependencies</arg>
      |              </args>
      |            </configuration>
      |          </execution>
      |        </executions>
      |        <configuration>
      |          <launchers>
      |              <launcher>
      |                  <id>main</id>
      |                  <mainClass>%s.Main</mainClass>
      |              </launcher>
      |          </launchers>
      |        </configuration>
      |      </plugin>
      |      <plugin>
      |        <groupId>org.apache.maven.plugins</groupId>
      |        <artifactId>maven-surefire-plugin</artifactId>
      |        <version>2.6</version>
      |        <configuration>
      |          <useFile>false</useFile>
      |          <disableXmlReport>true</disableXmlReport>
      |          <!-- If you have classpath issue like NoDefClassError,... -->
      |          <!-- useManifestOnlyJar>false</useManifestOnlyJar -->
      |          <includes>
      |            <include>**/*Test.*</include>
      |            <include>**/*Suite.*</include>
      |          </includes>
      |        </configuration>
      |      </plugin>
      |      <plugin>
      |      <artifactId>maven-assembly-plugin</artifactId>
      |        <configuration>
      |          <descriptorRefs>
      |            <descriptorRef>jar-with-dependencies</descriptorRef>
      |          </descriptorRefs>
      |            <archive>
      |              <manifest>
      |                <mainClass>%s.Main</mainClass>
      |                <addClasspath>true</addClasspath>
      |              </manifest>
      |            </archive>
      |        </configuration>
      |          <executions>
      |              <execution>
      |                  <id>make-assembly</id> <!-- this is used for inheritance merges -->
      |                  <phase>package</phase> <!-- bind to the packaging phase -->
      |                  <goals>
      |                      <goal>single</goal>
      |                  </goals>
      |              </execution>
      |          </executions>
      |      </plugin>
      |    </plugins>
      |  </build>
      |</project>
    """.stripMargin.format(groupId,artifactId,packagename,packagename)
}
