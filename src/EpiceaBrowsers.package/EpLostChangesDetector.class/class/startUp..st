system startup
startUp: isImageStarting

	(isImageStarting and: [ 
		self isEnabled and: [ "Detector can be disabled as a setting"
		EpMonitor current isEnabled	and: [ "Nothing to recover if not recording"
		Smalltalk isHeadless not ]]])  "Can't browse if UI disabled"
			ifFalse: [^self].

	self new openBrowserIfLostChanges