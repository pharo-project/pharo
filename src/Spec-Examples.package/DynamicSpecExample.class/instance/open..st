protocol
open: anObject
	anObject ifNil: [ ^ self openOnNil ].
	anObject isInteger
		ifTrue: [ ^ self openOnInteger: anObject ].
	anObject isString
		ifTrue: [ ^ self openOnString: anObject ]