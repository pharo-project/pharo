accessing
dependentPackagesFrom: aPackage
	"only the dependent packages from the cycle"
	^ aPackage dependentPackages select: [ :package |
		self packages includes: package
		 ]