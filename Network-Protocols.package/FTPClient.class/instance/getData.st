private protocol
getData

	| dataStream |
	dataStream := RWBinaryOrTextStream on: (String new: 4000).
	self getDataInto: dataStream.
	self closeDataSocket.
	^dataStream contents
