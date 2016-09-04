accessing
skipSeparatorsAndPeekNext

	"A special function to make nextChunk fast"
	| peek pos |
	[self atEnd] whileFalse: [
		pos := self position.
		(peek := self next) isSeparator ifFalse: [
			self position: pos.
			^ peek.
		].
	].
