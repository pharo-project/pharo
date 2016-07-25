private
classObjectFor: anObject 
	(anObject isBehavior or: [anObject isTrait]) ifTrue: [^self model classFor: anObject].
	anObject isSymbol ifTrue: [^self model classNamed: anObject].
	^anObject