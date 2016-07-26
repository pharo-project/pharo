subclass creation
immediateByteSubclass: className instanceVariableNames: instvarNames classVariableNames: classVarNames package: package
	"Added to allow for a simplified subclass creation experience. "

	^ self
		immediateSubclass: className
		instanceVariableNames: instvarNames
		classVariableNames: classVarNames
		package: package