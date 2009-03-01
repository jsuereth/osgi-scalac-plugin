package scala.osgi.compiler.phases

import tools.nsc.plugins._
import tools.nsc.backend.icode._
import tools.nsc._
import manifest._



  
  /**
   * This plugin makes sure that all referenced packages in the generated source of this compilation 
   * *could* be implemented in our OSGi manifest.   It does *not* enforce that things *are* imported
   * by our manifest
   */
class ImportChecker(val global : Global, val info: CompileTimeInformation) extends PluginComponent {
    val runsAfter : _root_.scala.List[String] = "icode" :: Nil
    val phaseName = "enforce-package-imports"
    def newPhase(prev : Phase) = new EnforcePackageImportsPhase(prev)
    def name = phaseName
    
    /** This phase rips into all the imports from icode and enforces they are ok...*/
    class EnforcePackageImportsPhase(prev:Phase) extends Phase(prev) {
      override def name = ImportChecker.this.name
      
      
	  /** Returns the . seperated package containing a symbol (or the symbol itself, if it is a package) */
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
	    if(!info.isPackageImportAllowed(pkg)) {
	      unit.error(symbol.pos,"Package "+pkg + " is not available in OSGi Container!")
	    }
	   }
		  
      override def run() {
        //Set one -> Add acceptable imports from current scala source
        for { unit <- global.currentRun.units
             if !unit.isJava
             icodeClass <- unit.icode          
        } {
          //Add 'generated' packages to export list
          val pkg = getPackageFor(icodeClass.symbol)
          info.addPackageExport(pkg)    
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
            //Check blocks for method calls, or new instantiation of invalid packages...
            import global.icodes.opcodes._
            for { 
              block <- method.code.blocks 
              insn <- block
            } insn match {
              case CALL_METHOD(symbol,_) =>
                enforceSymbolReferenceAllowed(symbol, unit)
              case NEW(kind) =>
                enforceSymbolReferenceAllowed(kind.cls, unit)                
              case _ =>
                //Ignore all other icode instructions!
            }
          }
        }
      }
    }  

}
