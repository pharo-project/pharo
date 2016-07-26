testing
atWordBoundary

	^(self isWordChar: self lastChar)
		xor: (self isWordChar: stream peek)