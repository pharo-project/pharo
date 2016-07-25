private
byteTextConverters
	^ ByteTextConverters ifNil: [ self initializeByteTextConverters ]