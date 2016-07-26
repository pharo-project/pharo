sending
sendSomeData: aStringOrByteArray startIndex: startIndex
	"Send as much of the given data as possible starting at the given index. Answer the number of bytes actually sent."
	"Note: This operation may have to be repeated multiple times to send a large amount of data."

	^ self
		sendSomeData: aStringOrByteArray
		startIndex: startIndex
		count: (aStringOrByteArray size - startIndex + 1)