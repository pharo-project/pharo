primitives
ffiTestCall: enumValue format: format to: buffer
	^ self 
		ffiCall: #( int sprintf ( ByteArray buffer, String format, FFITestEnumeration enumValue ) )
		module: LibC