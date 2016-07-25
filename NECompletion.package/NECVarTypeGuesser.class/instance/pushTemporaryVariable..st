bytecode decoding
pushTemporaryVariable: offset 
	"Push Contents Of Temporary Variable Whose Index Is the  
	argument, offset, On Top Of Stack bytecode."
	| info |
		contextCount > 0 ifTrue:[^self].

	info := NECTypeInfo definedByTemporaryVar: offset.
	types add: info