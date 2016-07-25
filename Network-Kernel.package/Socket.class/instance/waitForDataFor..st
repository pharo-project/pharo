waiting
waitForDataFor: timeout
	"Wait for the given nr of seconds for data to arrive.
	Signal a time out or connection close exception if either happens before data becomes available."

	^self
		waitForDataFor: timeout
		ifClosed: [ConnectionClosed signal: 'Connection closed while waiting for data.']
		ifTimedOut: [ConnectionTimedOut signal: 'Data receive timed out.']
