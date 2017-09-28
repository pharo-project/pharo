reflective operations
doesNotUnderstand: aMessage
	| selector variable setter|
		
	selector := aMessage selector.

	(selector isUnary or:[ selector isKeyword and:[ selector keywords size = 1] ]) 
		ifFalse:[ ^ super doesNotUnderstand: aMessage].

	setter := selector isKeyword.
	variable := setter ifTrue:[(selector allButLast: 1) asSymbol] ifFalse:[selector]. 
	
	(extensibleProperties includesKey: variable) 
		ifFalse: [ ^ super doesNotUnderstand: aMessage ].
	
	setter 
		ifTrue:[ extensibleProperties at: variable put: aMessage argument] 
		ifFalse:[ ^ extensibleProperties at: variable].
	