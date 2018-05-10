"
Tests for different FFI types
"
Class {
	#name : #FFITypesTests,
	#superclass : #TestCase,
	#category : #'UnifiedFFI-Tests-Tests'
}

{ #category : #ranges }
FFITypesTests >> signed1ByteNumbers [
	^ { 
		-128.
		-127.
		0.
		126.
		127
 	}
]

{ #category : #ranges }
FFITypesTests >> signed2ByteNumbers [
	^ { 
		-32768.
		-32767.
		-129.
		-128.
		-127.
		0.
		126.
		127.
		128.
		254.
		255.
		256.
		32766.
		32767
 	}
]

{ #category : #ranges }
FFITypesTests >> signed4ByteNumbers [
	^ { 
		-2147483648.
		-2147483647.
		-32769.
		-32768.
		-32767.
		-129.
		-128.
		-127.
		0.
		126.
		127.
		128.
		254.
		255.
		256.
		32766.
		32767.
		32768.
		65534.
		65535.
		65536.
		2147483646.
		2147483647.
 	}
]

{ #category : #ranges }
FFITypesTests >> signed8ByteNumbers [
	^ {
		-9223372036854775808.
		-9223372036854775807.
		-2147483649.
		-2147483648.
		-2147483647.
		-32769.
		-32768.
		-32767.
		-129.
		-128.
		-127.
		0.
		126.
		127.
		128.
		254.
		255.
		256.
		32766.
		32767.
		32768.
		65534.
		65535.
		65536.
		2147483646.
		2147483647.
		2147483648.
		9223372036854775806.
		9223372036854775807
 	}
]

{ #category : #tests }
FFITypesTests >> testSigned1ByteIntegers [
	self signed1ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 1.
		ref integerAt: 1 put: int size: 1 signed: true.
		self assert: (ref integerAt: 1 size: 1 signed: true) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testSigned2ByteIntegers [
	self signed2ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 2.
		ref integerAt: 1 put: int size: 2 signed: true.
		self assert: (ref integerAt: 1 size: 2 signed: true) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testSigned4ByteIntegers [
	self signed4ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 4.
		ref integerAt: 1 put: int size: 4 signed: true.
		self assert: (ref integerAt: 1 size: 4 signed: true) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testSignedAtWithDiferentSizes [
	"This is tests for issue 17757" 
	| ref|
	
	ref := ByteArray new: 4.
	ref integerAt: 1 put: -10 size: 1 signed: true.
	self assert: (ref integerAt: 1 size: 1 signed: true) equals: -10.
	
	ref := ByteArray new: 4.
	ref integerAt: 1 put: -10 size: 4 signed: true.
	self assert: (ref integerAt: 1 size: 4 signed: true) equals: -10.
	
	ref := ByteArray new: 8.
	ref integerAt: 1 put: -10 size: 8 signed: true.
	self assert: (ref integerAt: 1 size: 8 signed: true) equals: -10
]

{ #category : #tests }
FFITypesTests >> testSignedLong [
	self signed4ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIInt32 externalTypeSize.
		ref signedLongAt: 1 put: int.
		self assert: (ref signedLongAt: 1) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testSignedLongLong [
	self signed8ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIInt64 externalTypeSize.
		ref signedLongLongAt: 1 put: int.
		self assert: (ref signedLongLongAt: 1) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testUnsigned1ByteIntegers [
	self unsigned1ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 1.
		ref integerAt: 1 put: int size: 1 signed: false.
		self assert: (ref integerAt: 1 size: 1 signed: false) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testUnsigned2ByteIntegers [
	self unsigned2ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 2.
		ref integerAt: 1 put: int size: 2 signed: false.
		self assert: (ref integerAt: 1 size: 2 signed: false) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testUnsigned4ByteIntegers [
	self unsigned4ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: 4.
		ref integerAt: 1 put: int size: 4 signed: false.
		self assert: (ref integerAt: 1 size: 4 signed: false) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testUnsignedLong [
	self unsigned4ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIUInt32 externalTypeSize.
		ref unsignedLongAt: 1 put: int.
		self assert: (ref unsignedLongAt: 1) equals: int ]
]

{ #category : #tests }
FFITypesTests >> testUnsignedLongLong [
	self unsigned8ByteNumbers do: [ :int |
		|ref|
		ref := ByteArray new: FFIUInt64 externalTypeSize.
		ref unsignedLongLongAt: 1 put: int.
		self assert: (ref unsignedLongLongAt: 1) equals: int ]
]

{ #category : #ranges }
FFITypesTests >> unsigned1ByteNumbers [
	^ { 
		0.
		126.
		127.
		128.
		254.
		255
 	}
]

{ #category : #ranges }
FFITypesTests >> unsigned2ByteNumbers [
	^ { 
		0.
		126.
		127.
		128.
		254.
		255.
		256.
		32766.
		32767.
		32768.
		65534.
		65535
 	}
]

{ #category : #ranges }
FFITypesTests >> unsigned4ByteNumbers [
	^ { 
		0.
		126.
		127.
		128.
		254.
		255.
		256.
		32766.
		32767.
		32768.
		65534.
		65535.
		65536.
		2147483646.
		2147483647.
		2147483648.
		4294967294.
		4294967295
 	}
]

{ #category : #ranges }
FFITypesTests >> unsigned8ByteNumbers [
	^ { 
		0.
		126.
		127.
		128.
		254.
		255.
		256.
		32766.
		32767.
		32768.
		65534.
		65535.
		65536.
		2147483646.
		2147483647.
		2147483648.
		4294967294.
		4294967295.
		9223372036854775806.
		9223372036854775807.
		9223372036854775808.
		18446744073709551614.
		18446744073709551615
 	}
]
