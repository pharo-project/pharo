private
byteTextConverters
	^ byteTextConverters ifNil: [ self initializeByteTextConverters ]