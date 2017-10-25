character classification
isLetter: aCharacter

	^ (aCharacter between: $a and: $z)
		or: [ (aCharacter between: $A and: $Z) ]	