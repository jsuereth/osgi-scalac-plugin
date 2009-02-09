package scala.osgi.compiler

import tools.nsc.plugins._
import tools.nsc.backend.icode._
import tools.nsc._
import manifest._
class OsgiPlugin(val global : Global) extends Plugin {
  val name = "privateSetter"
  val description = "allows vars to have private setters"
  //TODO - See if we can turn off components/phases based on input settings...
  val components = List[PluginComponent](ManifestChecker,ImportChecker)  
  
  var allowedImports : collection.Set[String] = collection.immutable.Set()
  
  
   /** Plugin component to complete dependency analysis after a build and write out dependnecies for next build */
  private object ManifestChecker extends PluginComponent {
    val global = OsgiPlugin.this.global
    val runsAfter = "parser"
    val phaseName = "read-osgi-manifests"
    
    def newPhase(prev: Phase) = new phases.ReadBundleManifestsPhase(prev, global, allowedImports = _)
    def name = phaseName
  }
  
  

  
  
  private object ImportChecker extends PluginComponent {
    val global = OsgiPlugin.this.global
    val runsAfter = "icode"
    val phaseName = "enforce-package-imports"
    def newPhase(prev : Phase) = new EnforcePackageImportsPhase(prev)
    def name = phaseName
    
    /** This phase rips into all the imports from icode and enforces they are ok...*/
    class EnforcePackageImportsPhase(prev:Phase) extends Phase(prev) {
      override def name = ImportChecker.this.name
      
      
      /**
	   * Checks to see if a given class reference is ok.
	   */
	  def isReferenceAllowed(name : String) : Boolean = {
	    Console.println("Checking: " + name + " for viable OSGi usage")
	    def obtainPackageName(name : String) = {
	      val idx = name.lastIndexOf('.')
	      if(idx < 0) {
	        ""
	      } else {
	        name.substring(0, idx)
	      }
	    }
	    val pkgName = obtainPackageName(name)
	    //TOOD - are there others we should ignore?
	    pkgName.startsWith("java") || allowedImports.contains(pkgName ) || pkgName.equals("")
	  }
	  /** Returns the . seperated package containing (or that is) a symbol */
      def getPackageFor(symbol:global.Symbol) = if(symbol.isPackage) {
        symbol.fullNameString('.')
      } else {
        symbol.enclosingPackage.fullNameString('.')
      }
      /** 
       * This will make sure the symbol does not violate our package import rules.
       * As a side effect it will create an error on the source unit where the violated package is used.
       */
	  def enforceSymbolReferenceAllowed(symbol: global.Symbol, unit : global.CompilationUnit)  {
	    val pkg = getPackageFor(symbol)
	    if(!isReferenceAllowed(pkg)) {
	      unit.error(symbol.pos,"Package "+pkg + " is not available in OSGi Container!")
	    }
	   }
		  
      override def run() {
        //Set one -> Add acceptable imports from current scala source
        for {unit <- global.currentRun.units
             if !unit.isJava
             icodeClass <- unit.icode          
        } {
          //Add 'generated' packages to acceptable package imports
          val pkg = getPackageFor(icodeClass.symbol)
          Console.println("Adding package: " + pkg + " to approved import list")
          allowedImports = collection.immutable.HashSet(pkg) ++ allowedImports    
          Console.println("Approved Packages now = ")
          allowedImports.foreach(Console.println)
          ()
        }
        //TODO - Step two -> Add acceptable imports form current java source
        
        //Step 3 - Enforce restrictions on scala source.
        for (unit <- global.currentRun.units; icodeClass <- unit.icode) {
          //Enforcement on all the fields...
          for(field <- icodeClass.fields) {
            enforceSymbolReferenceAllowed(field.symbol, unit)            
          }
          //Enforcement on methods
          for(method <- icodeClass.methods) {
            //Enforce local variables are in correct packages
            for(local <- method.locals) {                
              enforceSymbolReferenceAllowed(local.sym, unit)
            }
            //Check blocks for static references
            /*for(block <- method.code.blocks;
                insn <- block) {
              for(consumed <- insn.consumedTypes) {
                enforceSymbolReferenceAllowed(consumed.toType.typeSymbolDirect, unit)                
              }
              for(produced <- insn.producedTypes) {
                enforceSymbolReferenceAllowed(produced.toType.typeSymbolDirect, unit)
              }
            }*/
          }
        }
        
      }
    }
  }
}
