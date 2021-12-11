Extension { #name : #FreeTypeCache }

{ #category : #'*FreeType-Graphics' }
FreeTypeCache >> sizeOf: anObject [
	^(anObject isKindOf: Form)
		ifTrue:[(anObject bitsSize * 4) + 32]
		ifFalse:[4]
]
