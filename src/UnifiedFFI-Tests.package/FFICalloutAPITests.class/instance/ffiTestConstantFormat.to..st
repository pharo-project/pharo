primitives constant
ffiTestConstantFormat: format to: buffer
	^ self ffiCall: #( int sprintf ( ByteArray buffer, String format, 65, 65, true ) )