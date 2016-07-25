private-class
classObjectFor: anObject 
	(anObject isBehavior or: [anObject isTrait]) ifTrue: [ ^ environment classFor: anObject ].
	anObject isSymbol ifTrue: [ ^ environment classNamed: anObject ].
	^ anObject