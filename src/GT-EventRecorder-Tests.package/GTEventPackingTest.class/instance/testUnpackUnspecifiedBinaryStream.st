tests-packing
testUnpackUnspecifiedBinaryStream
	self assertBundle.
	file writeStreamDo: [ :aStream | 
		aStream nextPutAll: bundle data ].
	file binaryReadStreamDo: [ :aStream | 
		announcement := packing unpack: aStream contents ].
	self assertUnpackedData.
