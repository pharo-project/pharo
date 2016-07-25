waiting
waitForDataIfClosed: closedBlock
	"Wait indefinitely for data to arrive.  This method will block until
	data is available or the socket is closed."

	[true]
		whileTrue: [
			(self primSocketReceiveDataAvailable: socketHandle)
				ifTrue: [^self].
			self isConnected
				ifFalse: [^closedBlock value].
			self readSemaphore wait].
