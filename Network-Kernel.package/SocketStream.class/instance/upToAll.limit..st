stream in
upToAll: aStringOrByteArray limit: nBytes
	"Answer a subcollection from the current access position to the occurrence (if any, but not inclusive) of aStringOrByteArray. If aCollection is not in the stream, or not found within nBytes answer the available contents of the stream"

	| index sz result searchedSoFar |
	sz := aStringOrByteArray size.
	"Look in the current inBuffer first"
	index := inBuffer indexOfSubCollection: aStringOrByteArray
						startingAt: lastRead - sz + 2.
	(index > 0 and: [(index + sz) <= inNextToWrite]) ifTrue: ["found it"
		result := self nextInBuffer: index - lastRead - 1.
		self skip: sz.
		^ result
	].

	[searchedSoFar :=  self inBufferSize.
	"Receive more data"
	self receiveData.
	recentlyRead > 0] whileTrue:[

		"Data begins at lastRead + 1, we add searchedSoFar as offset and 
		backs up sz - 1 so that we can catch any borderline hits."

		index := inBuffer indexOfSubCollection: aStringOrByteArray
						startingAt: (lastRead + searchedSoFar - sz + 2 max: 1).
		(index > 0 and: [(index + sz) <= inNextToWrite]) ifTrue: ["found it"
			result := self nextInBuffer: index - lastRead - 1.
			self skip: sz.
			^ result
		].
		"Check if we've exceeded the max. amount"
		(nBytes notNil and:[inNextToWrite - lastRead > nBytes]) 
			ifTrue:[^self nextAllInBuffer].
	].

	"Windows doesn't raise an Exception when connection is closed at first, so we check here the status of the connection"
	self shouldSignal ifTrue: [
		self isOtherEndConnected ifFalse: [ConnectionClosed signal: 'Connection closed while waiting for data.'].
	].

	"not found and (non-signaling) connection was closed"
	^self nextAllInBuffer