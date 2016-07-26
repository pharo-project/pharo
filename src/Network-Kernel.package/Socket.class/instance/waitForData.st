waiting
waitForData
	"Wait for data to arrive.  This method will block until
	data is available or the socket is closed.  If the socket is closed
	a ConnectionClosed exception will be signaled."

	^self waitForDataIfClosed:
		[ConnectionClosed signal: 'Connection close while waiting for data.']