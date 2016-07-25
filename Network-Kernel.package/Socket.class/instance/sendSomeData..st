sending
sendSomeData: aStringOrByteArray
	"Send as much of the given data as possible and answer the number of bytes actually sent."
	"Note: This operation may have to be repeated multiple times to send a large amount of data."

	^ self
		sendSomeData: aStringOrByteArray
		startIndex: 1
		count: aStringOrByteArray size