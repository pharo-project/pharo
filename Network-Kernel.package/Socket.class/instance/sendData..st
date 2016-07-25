sending
sendData: aStringOrByteArray
	"Send all of the data in the given array, even if it requires multiple calls to send it all. Return the number of bytes sent."

	"An experimental version use on slow lines: Longer timeout and smaller writes to try to avoid spurious timeouts."

	| bytesSent bytesToSend count |
	bytesToSend := aStringOrByteArray size.
	bytesSent := 0.
	[bytesSent < bytesToSend] whileTrue: [
		(self waitForSendDoneFor: 60)
			ifFalse: [ConnectionTimedOut signal: 'send data timeout; data not sent'].
		count := self primSocket: socketHandle
			sendData: aStringOrByteArray
			startIndex: bytesSent + 1
			count: (bytesToSend - bytesSent min: 5000).
		bytesSent := bytesSent + count].

	^ bytesSent
