*Monticello-RPackage
isDefinedAsPackageOrSubPackageInMC: aSymbol
	"a category has been added. "
	
	^ self allManagers anySatisfy: [ :each | 
		self 
			category: each packageName asSymbol 
			matches: aSymbol ]