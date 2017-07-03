subclass creation - immediate
immediateSubclass: aClassName uses: aTraitCompositionOrArray instanceVariableNames: instVarNames 
	classVariableNames: classVarNames package: cat
	"Added to allow for a simplified subclass creation experience. "
	
	^ self immediateSubclass: aClassName 
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames  
		classVariableNames: classVarNames 
		poolDictionaries: ''
		package: cat