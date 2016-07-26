public
sizeOf: anObject
	^(anObject isKindOf: Form)
		ifTrue:[(anObject bitsSize * 4) + 32]
		ifFalse:[4]
	