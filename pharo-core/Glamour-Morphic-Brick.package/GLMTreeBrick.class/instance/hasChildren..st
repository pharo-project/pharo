protocol
hasChildren: anObject

	anObject ifNil: [ ^ false ].
	anObject isVariableBinding ifFalse: [ ^ false ].
	anObject value ifNil: [ ^ false ].
	anObject value isCollection ifTrue: [ ^ anObject value isEmpty not ].
	^ true