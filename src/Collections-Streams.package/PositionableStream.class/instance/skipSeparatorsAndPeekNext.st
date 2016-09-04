accessing
skipSeparatorsAndPeekNext
	"A special function to make nextChunk fast"
	| peek |
	[self atEnd]
		whileFalse:
		[(peek := self next) isSeparator
			ifFalse: [self position: self position-1. ^ peek]]