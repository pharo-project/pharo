operations
removeNextCharacter
	| copy |
	copy := self copy.
	copy moveRight.
	^ (self to: copy) cut start