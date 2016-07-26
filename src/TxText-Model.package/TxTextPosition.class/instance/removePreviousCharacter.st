operations
removePreviousCharacter
	| copy |
	copy := self copy.
	copy moveLeft.
	^ (copy to: self) cut start