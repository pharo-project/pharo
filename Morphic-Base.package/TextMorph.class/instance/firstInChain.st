linked frames
firstInChain
	"Return the first morph in a chain of textMorphs"

	| first |
	first := self.
	[first predecessor isNil] whileFalse: [first := first predecessor].
	^first