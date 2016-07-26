accessing
messages
	messages isNil ifTrue: [self computeMessages].
	^messages