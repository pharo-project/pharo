private testing
responseIsContinuation: response
	^(response size > 3
		and: [(response at: 4) == $-])