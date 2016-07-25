bytecode decoding
prepare: aCompiledMethod 
	| theClass |
	super prepare: aCompiledMethod.
	theClass := aCompiledMethod realClass.
	varIndex := (theClass allInstVarNames indexOf: variableName) - 1