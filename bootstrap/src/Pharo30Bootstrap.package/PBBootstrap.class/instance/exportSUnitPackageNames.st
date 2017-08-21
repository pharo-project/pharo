preparation
exportSUnitPackageNames
	"Export list of kernel adn SUnit package names for RPackage initialization to a file"
		
	self exportPackageNamesFor: self kernelPackageNames, self sUnitPackageNames to: self bootstrapCacheDirectory / 'packagesSUnit.txt'.
	
	