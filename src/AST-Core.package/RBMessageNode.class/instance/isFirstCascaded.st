testing
isFirstCascaded
	^self isCascaded and: [parent messages first == self]