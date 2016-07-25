testing
atBeginningOfWord

	^(self isWordChar: self lastChar) not
		and: [self isWordChar: stream peek]