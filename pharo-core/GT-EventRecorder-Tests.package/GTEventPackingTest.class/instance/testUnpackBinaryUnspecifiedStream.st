tests-packing
testUnpackBinaryUnspecifiedStream
	self assertBundle.
	file writeStreamDo: [ :aStream | 
		aStream binary.
		aStream nextPutAll: bundle data ].
	announcement := packing unpack: file contents.
	self assertUnpackedData.