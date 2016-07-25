utility
exitFailure: aMessage 
	self hasSessionChanged
		ifTrue: [ ^ self ].
	Exit signalFailure: aMessage 