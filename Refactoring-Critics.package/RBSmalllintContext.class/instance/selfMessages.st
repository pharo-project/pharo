accessing
selfMessages
	selfMessages isNil ifTrue: [self computeMessages].
	^selfMessages