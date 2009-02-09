package scala.osgi.manifest


trait OSGIManifest  {
  def getImportedPackages : List[String]
  def getRequiredBundles : List[String]
  def getExportedPackages : List[String]
  def getClassPath : List[String]
  def getBundleSymbolicName : String
  def getBundleVersion : String
  def apply(key : String) : String
  def toString : String
}
/**
 * Helper objects with nice methods
 */
object OSGiManifestType {
  
  implicit def pimp(manifest:Manifest) = new OSGiManifestImpl(manifest)
  def apply(manifest : Manifest) : OSGIManifest = new OSGiManifestImpl(manifest)
  
  def unapply(manifest : Manifest) :Option[OSGIManifest] = {
    try {
     manifest("Bundle-ManifestVersion")
     // The above should throw if not an OSGi manifest... this is a pretty lame extractor...
     Some(pimp(manifest))
    } catch {
      case _ => None
    }
  }
}
/**
 * This class pimps a manifest with handy OSGi Methods...
 */
class OSGiManifestImpl(manifest : Manifest) extends OSGIManifest  {
  /** Removes all ;xyz=  froma  given string*/
  //TODO - Implement
  def removeQualifier(input : String) :String = input.replaceAll("""\s+""", "") //TODO - Remove ;xyz = for strings...
  
  
  override def getImportedPackages = {
    //TODO - Parse the import package key...
    manifest("Import-Package").split(",").map(removeQualifier).toList
  }
  override def getBundleVersion = removeQualifier(manifest("Bundle-Version"))
  override def getBundleSymbolicName = removeQualifier(manifest("Bundle-SymbolicName"))
   
  override def getRequiredBundles : List[String] = {
    manifest("Require-Bundle").split(",").map(removeQualifier).toList
  }
  override def getExportedPackages : List[String]  = {
    manifest("Export-Package").split(",").map(removeQualifier).toList
  }
  override def getClassPath : List[String]  = {
    manifest("Bundle-ClassPath").split(",").map(removeQualifier).toList
  }
  
  override def apply(key : String) = manifest.apply(key)
  
  override def toString = manifest.toString
}
