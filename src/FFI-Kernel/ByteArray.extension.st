Extension { #name : #ByteArray }

{ #category : #'*FFI-Kernel' }
ByteArray >> asExternalPointer [
	"Convert the receiver assuming that it describes a pointer to an object."
	^ self pointerAt: 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> booleanAt: byteOffset [
	"Returns the boolean the byte at index byteOffset of this ByteArray represents in the C convention ( A byte representing the 0 integer corresponds to false, while all other integers corresponds to true).
	Examples:
	(#[1 2 0 4] booleanAt: 2) >>> true.
	(#[1 2 0 4] booleanAt: 3) >>> false."
	
	^(self integerAt: byteOffset size: 1 signed: false) ~= 0
]

{ #category : #'*FFI-Kernel' }
ByteArray >> booleanAt: byteOffset put: value [
	"Booleans are just integers in C word"
	^self integerAt: byteOffset put: (value ifTrue:[1] ifFalse:[0]) size: 1 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> doubleAt: byteOffset [
	<primitive:'primitiveFFIDoubleAt' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'*FFI-Kernel' }
ByteArray >> doubleAt: byteOffset put: value [
	<primitive:'primitiveFFIDoubleAtPut' module:'SqueakFFIPrims'>
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #doubleAt:put: index: byteOffset value: value ].
	^self primitiveFailed
]

{ #category : #'*FFI-Kernel' }
ByteArray >> floatAt: byteOffset [
	<primitive:'primitiveFFIFloatAt' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'*FFI-Kernel' }
ByteArray >> floatAt: byteOffset put: value [
	<primitive:'primitiveFFIFloatAtPut' module:'SqueakFFIPrims'>
	self isReadOnlyObject 
		ifTrue: [ ^ self modificationForbiddenFor: #floatAt:put: index: byteOffset value: value ].
	^self primitiveFailed
]

{ #category : #'*FFI-Kernel' }
ByteArray >> integerAt: byteOffset put: value size: nBytes signed: aBoolean [
	"Primitive. Store the given value as integer of nBytes size
	in the receiver. Fail if the value is out of range.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	<primitive: 'primitiveFFIIntegerAtPut' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'*FFI-Kernel' }
ByteArray >> integerAt: byteOffset size: nBytes signed: aBoolean [
	"Primitive. Return an integer of nBytes size from the receiver.
	Note: This primitive will access memory in the outer space if
	invoked from ExternalAddress."
	<primitive: 'primitiveFFIIntegerAt' module:'SqueakFFIPrims'>
	^self primitiveFailed
]

{ #category : #'*FFI-Kernel' }
ByteArray >> isExternalAddress [
	"Return true if the receiver describes the address of an object in the outside world"
	^false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> isNull [
	"Answer false since only external addresses can be null"
	^false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> longPointerAt: byteOffset [
	"Answer an 8-byte pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress basicNew: 8.
	1 to: 8 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr
]

{ #category : #'*FFI-Kernel' }
ByteArray >> longPointerAt: byteOffset put: value [
	"Store an 8-byte pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 8 do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> pointerAt: byteOffset [
	"Answer a pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress new.
	1 to: ExternalAddress wordSize do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr
]

{ #category : #'*FFI-Kernel' }
ByteArray >> pointerAt: byteOffset put: value [
	"Store a pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: ExternalAddress wordSize do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> shortPointerAt: byteOffset [
	"Answer a 4-byte pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress basicNew: 4.
	1 to: 4 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^addr
]

{ #category : #'*FFI-Kernel' }
ByteArray >> shortPointerAt: byteOffset put: value [
	"Store a 4-byte pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 4 do:
		[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value basicAt: i)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedCharAt: byteOffset [
	^(self unsignedByteAt: byteOffset) asCharacter
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedCharAt: byteOffset put: aCharacter [
	^self unsignedByteAt: byteOffset put: aCharacter asciiValue
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongAt: byteOffset [
	"Return a 32bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset size: 4 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongAt: byteOffset put: value [
	"Store a 32bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 4 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongLongAt: byteOffset [
	| int |
	int := self unsignedLongLongAt: byteOffset.
	int > 16r7FFFFFFFFFFFFFFF ifTrue: [^int - 16r10000000000000000].
	^int
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongLongAt: byteOffset put: value [
	self unsignedLongLongAt: byteOffset put: (value < 0
		ifTrue: [ value + 16r10000000000000000 ]
		ifFalse: [ value ])
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedShortAt: byteOffset [
	"Return a 16bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset size: 2 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedShortAt: byteOffset put: value [
	"Store a 16bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 2 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> structAt: byteOffset length: length [
	"Return a structure of the given length starting at the indicated byte offset."
	| value |
	value := ByteArray new: length.
	1 to: length do:[:i|
		value unsignedByteAt: i put: (self unsignedByteAt: byteOffset+i-1)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> structAt: byteOffset put: value length: length [
	"Store a structure of the given length starting at the indicated byte offset."
	1 to: length do:[:i|
		self unsignedByteAt: byteOffset+i-1 put: (value unsignedByteAt: i)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedByteAt: byteOffset [
	"Return a 8bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset size: 1 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedByteAt: byteOffset put: value [
	"Store a 8bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 1 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedCharAt: byteOffset [
	^(self unsignedByteAt: byteOffset) asCharacter
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedCharAt: byteOffset put: aCharacter [
	^self unsignedByteAt: byteOffset put: aCharacter asciiValue
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongAt: byteOffset [
	"Return a 32bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset size: 4 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongAt: byteOffset put: value [
	"Store a 32bit signed integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 4 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongLongAt: byteOffset [
	"Answer a 64-bit integer in Smalltalk order (little-endian)."
	^self integerAt: byteOffset size: 8 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongLongAt: byteOffset put: value [
	"I store 64-bit integers in Smalltalk (little-endian) order."
	^self integerAt: byteOffset put: value size: 8 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedShortAt: byteOffset [
	"Return a 16bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset size: 2 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedShortAt: byteOffset put: value [
	"Store a 16bit unsigned integer starting at the given byte offset"
	^self integerAt: byteOffset put: value size: 2 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> voidAt: byteOffset [
	"no accessors for void"
	^self shouldNotImplement
]

{ #category : #'*FFI-Kernel' }
ByteArray >> voidAt: byteOffset put: value [
	"no accessors for void"
	^self shouldNotImplement
]
