tests-packing
testUnpackBinaryBinaryStream
	self assertBundle.
	file writeStreamDo: [ :aStream | 
		aStream binary.
		aStream nextPutAll: bundle data ].
	file binaryReadStreamDo: [ :aStream | 
		announcement := packing unpack: aStream contents ].
	self assertUnpackedData.
