primitives
ffiTestReturn: aNumber format: format to: buffer
	^ self 
		ffiCall: #( FFITestEnumeration sprintf ( ByteArray buffer, String format, int aNumber ) )
		module: LibC