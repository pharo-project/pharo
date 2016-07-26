variables
lookupVariableForWrite: aVariableNode

	| var |
	
	var := scope lookupVar: aVariableNode name.

	var ifNil: [^var].
	var isSpecialVariable ifTrue: [ self storeIntoSpecialVariable: aVariableNode ].
	var isWritable ifFalse: [ self storeIntoReadOnlyVariable: aVariableNode ].
	
	var isTemp ifTrue: [
		(var scope outerNotOptimizedScope ~= scope outerNotOptimizedScope) "only escaping when they will end up in different closures"
			ifTrue: [ var markEscapingWrite]].
	^var