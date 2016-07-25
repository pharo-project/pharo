primitives constant
ffiTestContantFormat: format value: aNumber to: buffer
	^ self ffiCall: #( int sprintf ( ByteArray buffer, String format, 65, 65, long aNumber ) )