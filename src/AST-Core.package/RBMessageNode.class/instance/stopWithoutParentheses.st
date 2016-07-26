accessing
stopWithoutParentheses
	^arguments isEmpty 
		ifTrue: [self keywordsIntervals first last]
		ifFalse: [arguments last stop]