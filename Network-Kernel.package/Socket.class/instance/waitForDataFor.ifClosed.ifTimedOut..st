waiting
waitForDataFor: timeout ifClosed: closedBlock ifTimedOut: timedOutBlock
	"Wait for the given nr of seconds for data to arrive."
	
	| startTime msecsDelta |
	startTime := Time millisecondClockValue.
	msecsDelta := (timeout * 1000) truncated.
	[(Time millisecondsSince: startTime) < msecsDelta] whileTrue: [
		(self primSocketReceiveDataAvailable: socketHandle)
			ifTrue: [^self].
		self isConnected
			ifFalse: [^closedBlock value].
		self readSemaphore waitTimeoutMSecs: 
			(msecsDelta - (Time millisecondsSince: startTime) max: 0).
	].

	(self primSocketReceiveDataAvailable: socketHandle)
		ifFalse: [
			self isConnected
				ifTrue: [^timedOutBlock value]
				ifFalse: [^closedBlock value]].