subclass creation
subclass: aSubclassSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames category: aCategorySymbol
	"Added to allow for a simplified subclass creation experience. "
	^ self
		subclass: aSubclassSymbol
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		poolDictionaries: ''
		category: aCategorySymbol