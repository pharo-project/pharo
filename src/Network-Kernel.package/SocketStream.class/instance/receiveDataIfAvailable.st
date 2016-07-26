private-socket
receiveDataIfAvailable
	"Only used to check that there really is data to read
	from the socket after it signals dataAvailable.
	It has been known to signal true and then still
	not have anything to read. See also isDataAvailable.
	Return the position in the buffer where the
	new data starts, regardless if anything
	was read, see #adjustInBuffer."

	recentlyRead := socket receiveSomeDataInto: inBuffer startingAt: inNextToWrite.
	^self adjustInBuffer: recentlyRead