private testing
responseIsContinuation
	^(self lastResponse size > 3
		and: [(self lastResponse at: 4) == $-])