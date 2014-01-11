package eu.lateral.nomic.mavenplugin;

import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import eu.lateral.nomic.meno.parser.Parser;
import eu.lateral.nomic.ASTObjects.ASTObject;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Goal which parses the meno file and runs all the code generation.
 *
 */
@Mojo( name = "meno", defaultPhase = LifecyclePhase.PROCESS_SOURCES )
public class Meno
    extends AbstractMojo
{
    /**
     * Location of the input file.
     */
    @Parameter( defaultValue = "${basedir}/dsl.meno", property = "inputFile", required = true )
    private File inputFile;

    public void execute()
        throws MojoExecutionException, MojoFailureException
    {
        File f = inputFile;

        if ( !f.exists() )
        {
            getLog().error( "Input file "+f.toString()+" does not exist." );
        }
        else{
		    String path = f.getAbsolutePath();
		    getLog().info( "Parsing "+path );
			ASTObject ast=(ASTObject)Parser.fromFile(f.getAbsolutePath());
			if (ast.error_message() != null){
              getLog().error(ast.error_message());
			  throw new MojoFailureException("Meno parsing error in "+f.toString());
			}
		}
    }
}
