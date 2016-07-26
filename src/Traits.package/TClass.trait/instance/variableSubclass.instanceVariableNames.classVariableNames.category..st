subclass creation - variable
variableSubclass: className instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat
	"Added to allow for a simplified subclass creation experience. "

	^ self variableSubclass: className
		instanceVariableNames: instVarNames 
		classVariableNames: classVarNames 
		poolDictionaries: ''
		category: cat
