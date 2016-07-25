required
readStreamForFileNamed: aString do: aBlock
	
	^ self clientDo:
		[:client | | stream |
		client binary.
		stream := RWBinaryOrTextStream on: String new.
		stream nextPutAll: (client getFileNamed: aString).
		aBlock value: stream reset]