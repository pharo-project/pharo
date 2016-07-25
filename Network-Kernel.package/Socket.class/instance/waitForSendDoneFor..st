waiting
waitForSendDoneFor: timeout
	"Wait up until the given deadline for the current send operation to complete. Return true if it completes by the deadline, false if not."

	| startTime msecsDelta msecsEllapsed sendDone |
	startTime := Time millisecondClockValue.
	msecsDelta := (timeout * 1000) truncated.
	[(sendDone := self primSocketSendDone: socketHandle) not and: [ self isConnected
			"Connection end and final data can happen fast, so test in this order"
		and: [(msecsEllapsed := Time millisecondsSince: startTime) < msecsDelta]]] whileTrue: [
			self writeSemaphore waitTimeoutMSecs: msecsDelta - msecsEllapsed].

	^ sendDone