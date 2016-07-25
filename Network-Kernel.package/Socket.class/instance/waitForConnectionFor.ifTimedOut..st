waiting
waitForConnectionFor: timeout ifTimedOut: timeoutBlock
	"Wait up until the given deadline for a connection to be established. Return true if it is established by the deadline, false if not."

	| startTime msecsDelta msecsEllapsed status |
	startTime := Time millisecondClockValue.
	msecsDelta := (timeout * 1000) truncated.
	status := self primSocketConnectionStatus: socketHandle.
	[(status = WaitingForConnection) and: [(msecsEllapsed := Time millisecondsSince: startTime) < msecsDelta]]
		whileTrue: [
			semaphore waitTimeoutMSecs: msecsDelta - msecsEllapsed.
			status := self primSocketConnectionStatus: socketHandle].

	status = Connected ifFalse: [^timeoutBlock value].
	^ true