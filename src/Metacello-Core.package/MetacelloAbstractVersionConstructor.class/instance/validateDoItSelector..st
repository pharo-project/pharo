validation
validateDoItSelector: anObject

	anObject == nil ifTrue: [ ^ self ].
	anObject isSymbol ifFalse: [ self error: 'Invalid message selector for doit: ', anObject printString ].