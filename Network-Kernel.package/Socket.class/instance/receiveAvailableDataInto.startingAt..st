receiving
receiveAvailableDataInto: buffer startingAt: startIndex
	"Receive all available data into the given buffer and return the number of bytes received.
	Note the given buffer may be only partially filled by the received data.
	Do not wait for data."

	| bufferPos bytesRead |
	bufferPos := startIndex.
	[self dataAvailable
		and: [bufferPos-1 < buffer size]] 
		whileTrue: [
			bytesRead := self receiveSomeDataInto: buffer startingAt: bufferPos.
			bufferPos := bufferPos + bytesRead].
	^bufferPos - startIndex