private-socket
receiveAvailableData
	"Receive available data (as much as fits in the inBuffer)
	but not waiting for more to arrive.
	Return the position in the buffer where the
	new data starts, regardless if anything
	was read, see #adjustInBuffer."
	
	recentlyRead := socket receiveAvailableDataInto: inBuffer startingAt: inNextToWrite.
	^self adjustInBuffer: recentlyRead