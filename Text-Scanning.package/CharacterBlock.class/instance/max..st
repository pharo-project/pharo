comparing
max: aCharacterBlock
	aCharacterBlock ifNil:[^self].
	^aCharacterBlock > self
		ifTrue:[ aCharacterBlock]
		ifFalse:[self].