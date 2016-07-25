instance creation
with: aCharacter
	| newCollection |
	aCharacter asInteger < 256
		ifTrue:[newCollection := ByteString new: 1]
		ifFalse:[newCollection := WideString new: 1].
	newCollection at: 1 put: aCharacter.
	^newCollection