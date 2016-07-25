waiting
waitForDisconnectionFor: timeout
	"Wait for the given nr of seconds for the connection to be broken.
	Return true if it is broken by the deadline, false if not.
	The client should know the connection is really going to be closed
	(e.g., because he has called 'close' to send a close request to the other end)
	before calling this method."

	| startTime msecsDelta status |
	startTime := Time millisecondClockValue.
	msecsDelta := (timeout * 1000) truncated.
	status := self primSocketConnectionStatus: socketHandle.
	[((status == Connected) or: [(status == ThisEndClosed)]) and:
	 [(Time millisecondsSince: startTime) < msecsDelta]] whileTrue: [
		self discardReceivedData.
		self readSemaphore waitTimeoutMSecs: 
			(msecsDelta - (Time millisecondsSince: startTime) max: 0).
		status := self primSocketConnectionStatus: socketHandle].
	^ status ~= Connected