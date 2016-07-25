code generation (closures)
removeTempNode: aTempVariableNode ifAbsent: aBlock
	"Utilities for when we want to remove some temporaries."
	
	self makeTemporariesRemovable.
	^temporaries remove: aTempVariableNode ifAbsent: aBlock
	