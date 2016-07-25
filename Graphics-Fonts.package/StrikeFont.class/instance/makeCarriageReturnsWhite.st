character shapes
makeCarriageReturnsWhite
	"Some larger fonts have a gray carriage return (from the zero wide fixup) make it white so it doesn't show"
	| crForm |
	crForm := self characterFormAt: 13 asCharacter.
	crForm fillWhite.
	self 
		characterFormAt: 13 asCharacter
		put: crForm