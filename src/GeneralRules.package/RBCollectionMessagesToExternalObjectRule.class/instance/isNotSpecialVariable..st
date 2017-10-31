helpers
isNotSpecialVariable: variableNode

	(#('self' 'super') includes: variableNode name) ifTrue: [ ^false ]. 
	(Smalltalk globals includesKey: variableNode name asSymbol) ifTrue: [ ^ false ].
	^ true
	