subclass creation - variableWord
variableWordSubclass: className uses: aTraitCompositionOrArray instanceVariableNames: instVarNames classVariableNames: classVarNames category: cat
	"Added to allow for a simplified subclass creation experience. "

	^ self
		variableWordSubclass: className
		uses: aTraitCompositionOrArray
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		package: cat