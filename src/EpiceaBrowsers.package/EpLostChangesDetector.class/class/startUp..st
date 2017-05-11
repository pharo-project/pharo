system startup
startUp: isImageStarting

	| detector |
	(isImageStarting and: [ 
		self isEnabled and: [ "Detector can be disabled via system settings"
		EpMonitor current isEnabled	and: [ "Nothing to recover if wasn't recording"
		Smalltalk isHeadless not ]]])  "Can't browse if UI disabled"
			ifFalse: [ ^ self ].

	detector := self new.
	
	SessionManager default addDeferredStartupAction: [
		(detector hasLostChanges and: [ self confirm: 'It seems your last Pharo session exited without saving some code. Do you want to recover it?' ])
			ifTrue: [ detector openBrowserIfLostChanges ] ]
