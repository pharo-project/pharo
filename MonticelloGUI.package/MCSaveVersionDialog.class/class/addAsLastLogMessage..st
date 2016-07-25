history
addAsLastLogMessage: aString

	((self previousMessages size > 0) and: [self previousMessages first = aString])
		ifTrue: [ ^ self ].
		
	self previousMessages addFirst: aString.
	(self previousMessages size > self maxLogMessageHistory)
		ifTrue: [self previousMessages removeLast]