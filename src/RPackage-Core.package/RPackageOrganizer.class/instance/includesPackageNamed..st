testing
includesPackageNamed: aSymbol
	"Answer whether the receiver get a package named aSymbol as registered."
	^ packages includesKey:  aSymbol asSymbol