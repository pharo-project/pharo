testing
containedBy: anInterval 
	^anInterval first <= self startWithoutParentheses 
		and: [anInterval last >= self stopWithoutParentheses]