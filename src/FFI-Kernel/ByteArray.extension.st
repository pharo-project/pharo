Extension { #name : #ByteArray }

{ #category : #'*FFI-Kernel' }
ByteArray >> asExternalPointer [
	"Convert the receiver assuming that it describes a pointer to an object."
	^ self pointerAt: 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> boolean8AtOffset: zeroBasedOffset [
	<primitive: 600>
	^ (self integerAt: zeroBasedOffset + 1 size: 1 signed: false) ~= 0
]

{ #category : #'*FFI-Kernel' }
ByteArray >> boolean8AtOffset: zeroBasedOffset put: value [
	<primitive: 615>
	^ self integerAt: zeroBasedOffset + 1 put: (value ifTrue:[1] ifFalse:[0]) size: 1 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> booleanAt: byteIndex [
	"Returns the boolean the byte at index byteOffset of this ByteArray represents in the C convention ( A byte representing the 0 integer corresponds to false, while all other integers corresponds to true)."
	
	"(#[1 2 0 4] booleanAt: 2) >>> true."
	"(#[1 2 0 4] booleanAt: 3) >>> false."
	
	^ self boolean8AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> booleanAt: byteIndex put: value [
	^ self boolean8AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> char16AtOffset: zeroBasedOffset [
	<primitive: 611>
	^ (self integerAt: zeroBasedOffset + 1 size: 2 signed: false) asCharacter
]

{ #category : #'*FFI-Kernel' }
ByteArray >> char16AtOffset: zeroBasedOffset put: value [
	<primitive: 626>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 2 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> char32AtOffset: zeroBasedOffset [
	<primitive: 612>
	^ (self integerAt: zeroBasedOffset + 1 size: 4 signed: false) asCharacter
]

{ #category : #'*FFI-Kernel' }
ByteArray >> char32AtOffset: zeroBasedOffset put: value [
	<primitive: 627>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 4 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> char8AtOffset: zeroBasedOffset [
	<primitive: 610>
	^ (self integerAt: zeroBasedOffset + 1 size: 1 signed: false) asCharacter
]

{ #category : #'*FFI-Kernel' }
ByteArray >> char8AtOffset: zeroBasedOffset put: value [
	<primitive: 625>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 1 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> doubleAt: byteIndex [
	^ self float64AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> doubleAt: byteIndex put: value [
	^ self float64AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> float32AtOffset: zeroBasedOffset [
	<primitive: 613>

	^ FFIBackend current on: self float32At: zeroBasedOffset + 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> float32AtOffset: zeroBasedOffset put: value [
	<primitive: 628>
	
	^ FFIBackend current on: self float32At: zeroBasedOffset + 1 put: value

]

{ #category : #'*FFI-Kernel' }
ByteArray >> float64AtOffset: zeroBasedOffset [
	<primitive: 614>
	
	^ FFIBackend current on: self float64At: zeroBasedOffset + 1

]

{ #category : #'*FFI-Kernel' }
ByteArray >> float64AtOffset: zeroBasedOffset put: value [

	<primitive: 629>
	^ FFIBackend current
		  on: self
		  float64At: zeroBasedOffset + 1
		  put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> floatAt: byteIndex [
	^ self float32AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> floatAt: byteIndex put: value [
	^ self float32AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int16AtOffset: zeroBasedOffset [
	<primitive: 604>
	^ self integerAt: zeroBasedOffset + 1 size: 2 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int16AtOffset: zeroBasedOffset put: value [
	<primitive: 619>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 2 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int32AtOffset: zeroBasedOffset [
	<primitive: 606>
	^ self integerAt: zeroBasedOffset + 1 size: 4 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int32AtOffset: zeroBasedOffset put: value [
	<primitive: 621>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 4 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int64AtOffset: zeroBasedOffset [
	<primitive: 608>
	^ self integerAt: zeroBasedOffset + 1 size: 8 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int64AtOffset: zeroBasedOffset put: value [
	<primitive: 623>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 8 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int8AtOffset: zeroBasedOffset [
	<primitive: 602>
	^ self integerAt: zeroBasedOffset + 1 size: 1 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> int8AtOffset: zeroBasedOffset put: value [
	<primitive: 617>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 1 signed: true
]

{ #category : #'*FFI-Kernel' }
ByteArray >> integerAt: byteOffset put: value size: nBytes signed: aBoolean [

	^ FFIBackend current
		  integerOfObject: self
		  at: byteOffset
		  put: value
		  size: nBytes
		  signed: aBoolean
]

{ #category : #'*FFI-Kernel' }
ByteArray >> integerAt: byteOffset size: nBytes signed: aBoolean [

	^ FFIBackend current
		  integerOfObject: self
		  at: byteOffset
		  size: nBytes
		  signed: aBoolean
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
ByteArray >> longPointerAt: byteIndex [
	"Answer an 8-byte pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress basicNew: 8.
	1 to: 8 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteIndex+i-1)].
	^addr
]

{ #category : #'*FFI-Kernel' }
ByteArray >> longPointerAt: byteIndex put: value [
	"Store an 8-byte pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 8 do:
		[:i|
		self unsignedByteAt: byteIndex+i-1 put: (value basicAt: i)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> pointerAt: byteIndex [
	^ self pointerAtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> pointerAt: byteIndex put: value [
	^ self pointerAtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> pointerAtOffset: zeroBasedOffset [
	<primitive: 609>
	"Answer a pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress new.
	1 to: ExternalAddress wordSize do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: zeroBasedOffset+i)].
	^addr
]

{ #category : #'*FFI-Kernel' }
ByteArray >> pointerAtOffset: zeroBasedOffset put: value [
	<primitive: 624>
	"Store a pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: ExternalAddress wordSize do:
		[:i|
		self unsignedByteAt: zeroBasedOffset+i put: (value basicAt: i)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> shortPointerAt: byteIndex [
	"Answer a 4-byte pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress basicNew: 4.
	1 to: 4 do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: byteIndex+i-1)].
	^addr
]

{ #category : #'*FFI-Kernel' }
ByteArray >> shortPointerAt: byteIndex put: value [
	"Store a 4-byte pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: 4 do:
		[:i|
		self unsignedByteAt: byteIndex+i-1 put: (value basicAt: i)].
	^value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedCharAt: byteOffset [
	^ self char8AtOffset: byteOffset - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedCharAt: byteIndex put: aCharacter [
	^ self char8AtOffset: byteIndex - 1 put: aCharacter
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongAt: byteIndex [
	^ self int32AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongAt: byteIndex put: value [
	^ self int32AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongLongAt: byteIndex [
	^ self int64AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedLongLongAt: byteIndex put: value [
	^ self int64AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedShortAt: byteIndex [
	^ self int16AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> signedShortAt: byteIndex put: value [
	^ self int16AtOffset: byteIndex - 1 put: value
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
ByteArray >> uint16AtOffset: zeroBasedOffset [
	<primitive: 603>
	^ self integerAt: zeroBasedOffset + 1 size: 2 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> uint16AtOffset: zeroBasedOffset put: value [
	<primitive: 618>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 2 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> uint32AtOffset: zeroBasedOffset [
	<primitive: 605>
	^ self integerAt: zeroBasedOffset + 1 size: 4 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> uint32AtOffset: zeroBasedOffset put: value [
	<primitive: 620>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 4 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> uint64AtOffset: zeroBasedOffset [
	<primitive: 607>
	^ self integerAt: zeroBasedOffset + 1 size: 8 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> uint64AtOffset: zeroBasedOffset put: value [
	<primitive: 622>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 8 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> uint8AtOffset: zeroBasedOffset [
	<primitive: 601>
	^ self integerAt: zeroBasedOffset + 1 size: 1 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> uint8AtOffset: zeroBasedOffset put: value [
	<primitive: 616>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 1 signed: false
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedByteAt: byteIndex [
	^ self uint8AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedByteAt: byteIndex put: value [
	^ self uint8AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedCharAt: byteIndex [
	^ self char8AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedCharAt: byteIndex put: aCharacter [
	^ self char8AtOffset: byteIndex - 1 put: aCharacter
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongAt: byteIndex [
	^ self uint32AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongAt: byteIndex put: value [
	^ self uint32AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongLongAt: byteIndex [
	^ self uint64AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedLongLongAt: byteIndex put: value [
	^ self uint64AtOffset: byteIndex - 1 put: value
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedShortAt: byteIndex [
	^ self uint16AtOffset: byteIndex - 1
]

{ #category : #'*FFI-Kernel' }
ByteArray >> unsignedShortAt: byteIndex put: value [
	^ self uint16AtOffset: byteIndex - 1 put: value
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
