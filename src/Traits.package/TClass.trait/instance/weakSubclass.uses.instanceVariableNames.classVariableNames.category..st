subclass creation - weak
weakSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames category: cat
	"Added to allow for a simplified subclass creation experience. "
	
	^ self weakSubclass: className
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat
