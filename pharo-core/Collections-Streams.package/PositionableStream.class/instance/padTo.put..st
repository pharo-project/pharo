positioning
padTo: nBytes put: aCharacter 
	"Pad using the argument, aCharacter, to the next boundary of nBytes characters."
	| rem |
	rem := nBytes - (self position \\ nBytes).
	rem = nBytes ifTrue: [^ 0].
	self next: rem put: aCharacter.