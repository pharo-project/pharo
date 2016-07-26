subclass creation - variableByte
variableByteSubclass: className instanceVariableNames: instvarNames classVariableNames: classVarNames package: cat
	"Added to allow for a simplified subclass creation experience. "

	^ self
		variableByteSubclass: className
		instanceVariableNames: instvarNames
		classVariableNames: classVarNames
		category: cat