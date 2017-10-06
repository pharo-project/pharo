subclass creation - variableByte
variableByteSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames category: cat
		"Added to allow for a simplified subclass creation experience. "
	
	^ self variableByteSubclass: className
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat