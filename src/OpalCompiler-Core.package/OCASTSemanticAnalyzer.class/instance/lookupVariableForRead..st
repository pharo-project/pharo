variables
lookupVariableForRead: aVariableNode

	| var |
	var := scope lookupVar: aVariableNode name.
	var ifNil: [^var].
	(var isTemp and: [var scope outerNotOptimizedScope ~= scope outerNotOptimizedScope] )  "only escaping when they will end up in different closures"
		ifTrue: [var markEscapingRead].
	^var