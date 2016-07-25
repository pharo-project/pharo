public registration
registerPackageNamed: aString 
	^ self 
		packageNamed: aString asSymbol
		ifAbsent: [ (RPackage named: aString asSymbol) register ]