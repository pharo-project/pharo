preparation
exportKernelPackageNames
	"Export list of kernel package names for RPackage initialization to a file"
		
	self exportPackageNamesFor: self kernelPackageNames to: self bootstrapCacheDirectory / 'packagesKernel.txt'.
	
	