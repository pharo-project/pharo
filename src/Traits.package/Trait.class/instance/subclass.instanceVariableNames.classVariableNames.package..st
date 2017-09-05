subclass creation
subclass: aSubclassSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames package: aPackageSymbol
	"Added to allow for a simplified subclass creation experience. "

	^ self
		subclass: aSubclassSymbol
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		package: aPackageSymbol