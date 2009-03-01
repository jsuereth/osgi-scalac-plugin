package scala.osgi.compiler

trait CompileTimeInformation {

  def addPackageExport(pkg : String) : Unit
  def addAllowedPackageImport(pkg:String) : Unit
  def isPackageImportAllowed(pkg : String) : Boolean
}
