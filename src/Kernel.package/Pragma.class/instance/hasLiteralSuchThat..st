testing
hasLiteralSuchThat: aBlock
	"Answer true if litBlock returns true for any literal in the receiver, even if embedded in further array structure.
	 This method is only intended for private use by CompiledMethod hasLiteralSuchThat:"
	^(aBlock value: selector)
	   or: [arguments hasLiteralSuchThat: aBlock]