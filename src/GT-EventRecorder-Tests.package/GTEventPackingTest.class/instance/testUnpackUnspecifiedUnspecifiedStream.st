tests-packing
testUnpackUnspecifiedUnspecifiedStream
	self assertBundle.
	file writeStreamDo: [ :aStream | 
		aStream nextPutAll: bundle data ].
	announcement := packing unpack: file contents.
	self assertUnpackedData.