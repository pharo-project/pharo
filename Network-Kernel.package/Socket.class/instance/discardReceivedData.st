receiving
discardReceivedData
	"Discard any data received up until now, and return the number of bytes discarded."

	| buf totalBytesDiscarded |
	buf := String new: 10000.
	totalBytesDiscarded := 0.
	[self isConnected] whileTrue: [
		totalBytesDiscarded :=
			totalBytesDiscarded + (self receiveDataInto: buf)].
	^ totalBytesDiscarded
