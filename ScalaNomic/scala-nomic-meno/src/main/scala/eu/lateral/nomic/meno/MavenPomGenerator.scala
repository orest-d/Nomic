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
      |  <inceptionYear>2013</inceptionYear>
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
      |    <scala.version>2.10.0</scala.version>
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
      |      <version>4.11</version>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>org.scala-tools.testing</groupId>
      |      <artifactId>specs_2.10</artifactId>
      |      <version>1.6.9</version>
      |      <scope>test</scope>
      |    </dependency>
      |    <dependency>
      |      <groupId>org.scalatest</groupId>
      |      <artifactId>scalatest_2.10</artifactId>
      |      <version>2.0.M5b</version>
      |      <scope>test</scope>
      |    </dependency>
      |  </dependencies>
      |
      |  <build>
      |    <sourceDirectory>src/main/scala</sourceDirectory>
      |    <testSourceDirectory>src/test/scala</testSourceDirectory>
      |    <plugins>
      |      <plugin>
      |        <groupId>net.alchim31.maven</groupId>
      |        <artifactId>scala-maven-plugin</artifactId>
      |        <version>3.1.6</version>
      |        <executions>
      |          <execution>
      |            <goals>
      |              <goal>compile</goal>
      |              <goal>testCompile</goal>
      |            </goals>
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
