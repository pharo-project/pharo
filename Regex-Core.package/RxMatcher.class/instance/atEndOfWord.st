testing
atEndOfWord

	^(self isWordChar: self lastChar)
		and: [(self isWordChar: stream peek) not]