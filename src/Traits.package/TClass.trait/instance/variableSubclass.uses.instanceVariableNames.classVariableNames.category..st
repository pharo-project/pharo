subclass creation - variable
variableSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames category: cat
	"Added to allow for a simplified subclass creation experience. "
	
	^ self variableSubclass: aClassName 
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames  
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat