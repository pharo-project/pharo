required
writeStreamForFileNamed: aString replace: ignoreBoolean do: aBlock
	| stream |
	stream := RWBinaryOrTextStream on: String new.
	aBlock value: stream.
	self clientDo:
		[:client |
		client binary.
		client putFileStreamContents: stream reset as: aString]