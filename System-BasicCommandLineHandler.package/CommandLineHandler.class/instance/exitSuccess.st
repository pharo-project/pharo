utility
exitSuccess
	self hasSessionChanged
		ifTrue: [ ^ self ].
	Exit signalSuccess 