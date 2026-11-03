"
An ExternalAddress is an opaque handle to objects outside Pharo memory (e.g., a pointer).
"
Class {
	#name : 'ExternalAddress',
	#superclass : 'ByteArray',
	#type : 'bytes',
	#classInstVars : [
		'wordSize'
	],
	#category : 'FFI-Kernel',
	#package : 'FFI-Kernel'
}

{ #category : 'instance creation' }
ExternalAddress class >> allocate: byteSize [

	^ FFIBackend current allocate: byteSize
]

{ #category : 'instance creation' }
ExternalAddress class >> allocate: firstByteSize allocate: secondByteSize during: aBlock [

	| firstAddress secondAddress |
	firstAddress := self allocate: firstByteSize.
	secondAddress := self allocate: secondByteSize.

	^ [ aBlock value: firstAddress value: secondAddress ] ensure: [
		  firstAddress free.
		  secondAddress free ]
]

{ #category : 'instance creation' }
ExternalAddress class >> allocate: byteSize bytesDuring: aBlock [

	| address |
	address := self allocate: byteSize.
	^ [ aBlock value: address ] ensure: [ address free ]
]

{ #category : 'instance creation' }
ExternalAddress class >> allocateMemoryOfSizes: aCollectionOfSizes during: aBlock [

	| addresses |

	addresses := aCollectionOfSizes collect: [ :aSize | self allocate: aSize ] as: Array.

	^ [ aBlock valueWithArguments: addresses ]
		  ensure: [ addresses do: [ :anAddress | anAddress free ] ]
]

{ #category : 'instance creation' }
ExternalAddress class >> gcallocate: byteSize [
	"Primitive. Allocate an object on the external heap.
	The external memory will be freed when i am garbage collected.
	BEWARE: there should be no copy of self, nor any pointer to a sub part..."

	| externalAddress |
	externalAddress := self allocate: byteSize.
	self finalizationRegistry add: externalAddress.
	^externalAddress
]

{ #category : 'instance creation' }
ExternalAddress class >> loadSymbol: moduleSymbol module: module [

	^ FFIBackend current loadSymbol: moduleSymbol module: module
]

{ #category : 'instance creation' }
ExternalAddress class >> new [
	"External addresses are either four or eight bytes long."
	^super new: self wordSize
]

{ #category : 'instance creation' }
ExternalAddress class >> new: n [
	"Only create ExternalAddresses of the right size."
	^n = self wordSize
		ifTrue: [super new: n]
		ifFalse: [self shouldNotImplement]
]

{ #category : 'instance creation' }
ExternalAddress class >> null [
	^ self new
]

{ #category : 'system startup' }
ExternalAddress class >> startUp: resuming [
	"If starting the image afresh all external addresses should be zero.
	 In addition, if the word size has changed then external addresses shoiuld be resized.
	 The two steps are combined for efficiency."
	resuming ifTrue: [
		self allInstancesDo: [ :each | each beNull ]]
]

{ #category : 'accessing' }
ExternalAddress class >> wordSize [
	^wordSize ifNil: [ wordSize := Smalltalk wordSize ]
]

{ #category : 'arithmetic' }
ExternalAddress >> + offset [
	"Create an address that is offset by the given number of bytes.
	More tricky than one would think due to the FFI's handling of ExternalAddress
	as pointer to an object so that 'self unsignedLongAt: ' would dereference."

	| bytes |
	"Convert xaddr -> bytes"
	bytes := self asByteArrayPointer.
	"Update bytes using platform dependent accessors"
	self size = 4
		ifTrue: [bytes unsignedLongAt: 1 put: (bytes unsignedLongAt: 1) + offset]
		ifFalse: [bytes unsignedLongLongAt: 1 put: (bytes unsignedLongLongAt: 1) + offset].
	"Convert bytes -> xaddr"
	^bytes asExternalPointer
]

{ #category : 'comparing' }
ExternalAddress >> = other [

	self == other ifTrue: [ ^ true ].
	self species == other species ifFalse: [ ^ false ].
	1 to: self size do: [ :index |
		(self at: index) = (other at: index) ifFalse: [ ^ false ] ].

	^ true
]

{ #category : 'private' }
ExternalAddress >> asByteArrayPointer [
	"Answer a ByteArray containing a copy of pointer to the contents of the receiver."
	| sz |
	^(ByteArray basicNew: (sz := self size))
		replaceFrom: 1 to: sz with: self startingAt: 1 "answers self"
]

{ #category : 'converting' }
ExternalAddress >> asExternalAddress [

	^ self
]

{ #category : 'private' }
ExternalAddress >> asExternalPointer [
	"No need to convert."
	^self
]

{ #category : 'converting' }
ExternalAddress >> asInteger [
	"convert address to integer"
	^ self asByteArrayPointer integerAt: 1 size: self size signed: false
]

{ #category : 'initialization' }
ExternalAddress >> beNull [
	"Make the receiver a NULL pointer"
	self atAllPut: 0
]

{ #category : 'accessing' }
ExternalAddress >> boolean8AtOffset: zeroBasedOffset [
	<primitive: 630>
	^ (self integerAt: zeroBasedOffset + 1 size: 1 signed: false) ~= 0
]

{ #category : 'accessing' }
ExternalAddress >> boolean8AtOffset: zeroBasedOffset put: value [
	<primitive: 645>
	^ self integerAt: zeroBasedOffset + 1 put: (value ifTrue:[1] ifFalse:[0]) size: 1 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> booleanAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self boolean8AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> booleanAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self boolean8AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> byteAt: byteOffset [
	"Go through a different primitive since the receiver describes data in the outside world"
	^self unsignedByteAt: byteOffset
]

{ #category : 'accessing' }
ExternalAddress >> byteAt: byteOffset put: value [
	"Go through a different primitive since the receiver describes data in the outside world"
	^self unsignedByteAt: byteOffset put: value
]

{ #category : 'accessing' }
ExternalAddress >> bytesFromCString [
	" Assume that the receiver represents a C string and convert it to a byte array.
	WARNING: the referenced data MUST ends with a NULL character (byte 0).
	To obtain a Smalltalk String, the byte array has to be decoded with the right decoder.
	ex: extAdr asByteArray decodeWith: 'ISO-8859-2'
	"
	| index aByte |

	^ ByteArray streamContents: [ :aStream |
			index := 1.
			[(aByte := self unsignedByteAt: index) = 0]
				whileFalse: [
					aStream nextPut: aByte.
					index := index + 1]]
]

{ #category : 'accessing' }
ExternalAddress >> char16AtOffset: zeroBasedOffset [
	<primitive: 641>
	^ (self integerAt: zeroBasedOffset + 1 size: 2 signed: false) asCharacter
]

{ #category : 'accessing' }
ExternalAddress >> char16AtOffset: zeroBasedOffset put: value [
	<primitive: 656>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 2 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> char32AtOffset: zeroBasedOffset [
	<primitive: 642>
	^ (self integerAt: zeroBasedOffset + 1 size: 4 signed: false) asCharacter
]

{ #category : 'accessing' }
ExternalAddress >> char32AtOffset: zeroBasedOffset put: value [
	<primitive: 657>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 4 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> char8AtOffset: zeroBasedOffset [
	<primitive: 640>
	^ (self integerAt: zeroBasedOffset + 1 size: 1 signed: false) asCharacter
]

{ #category : 'accessing' }
ExternalAddress >> char8AtOffset: zeroBasedOffset put: value [
	<primitive: 655>
	^ self integerAt: zeroBasedOffset + 1 put: value asInteger size: 1 signed: false
]

{ #category : 'copying' }
ExternalAddress >> clone [
	
	<primitive: 148>
	self deprecated: 'Use shallowCopy'.
	self primitiveFailed.
	
]

{ #category : 'copying' }
ExternalAddress >> copyFrom: startIndex to: endIndex [
	| result size |
	size := endIndex - startIndex + 1.
	result := ByteArray new: size.
	LibC memCopy: (self + (startIndex - 1)) to: result size: size.
	^ result
]

{ #category : 'copying' }
ExternalAddress >> copyFromOffset: startZeroBasedOffset to: endZeroBasedOffset [
	^ self copyFrom: startZeroBasedOffset + 1 to: endZeroBasedOffset + 1
]

{ #category : 'accessing' }
ExternalAddress >> doubleAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float64AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> doubleAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float64AtOffset: byteIndex - 1 put: value
]

{ #category : 'finalization' }
ExternalAddress >> finalize [
	"I am an executor (a copy) of an ExternalAddress that was just garbage collected.
	I must finalize. my mission is to free memory"
	self isNull ifTrue: [^self].
	self free
]

{ #category : 'accessing' }
ExternalAddress >> float32AtOffset: zeroBasedOffset [
	<primitive: 643>

	^ FFIBackend current on: self float32At: zeroBasedOffset + 1
]

{ #category : 'accessing' }
ExternalAddress >> float32AtOffset: zeroBasedOffset put: value [
	<primitive: 658>

	^ FFIBackend current on: self float32At: zeroBasedOffset + 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> float64AtOffset: zeroBasedOffset [
	<primitive: 644>

	^ FFIBackend current on: self float64At: zeroBasedOffset + 1
]

{ #category : 'accessing' }
ExternalAddress >> float64AtOffset: zeroBasedOffset put: value [

	<primitive: 659>
	^ FFIBackend current
		  on: self
		  float64At: zeroBasedOffset + 1
		  put: value
]

{ #category : 'accessing' }
ExternalAddress >> floatAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float32AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> floatAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self float32AtOffset: byteIndex - 1 put: value
]

{ #category : 'initialization' }
ExternalAddress >> free [

	^ FFIBackend current free: self
]

{ #category : 'converting' }
ExternalAddress >> fromInteger: anInteger [
	"Update me with the received address, which is an integer.
	Warning: The implementation assumes the platform is little-endian."

	| tmp |
	tmp := anInteger.
	"Set each byte except last"
	1 to: self size - 1 do: [ :i |
		self basicAt: i put: (tmp bitAnd: 16rff).
		tmp := tmp bitShift: -8 ].
	"Last byte doesn't need any additional bit operation"
	self basicAt: self size put: tmp
]

{ #category : 'accessing' }
ExternalAddress >> getHandle [

	^ self
]

{ #category : 'accessing' }
ExternalAddress >> int16AtOffset: zeroBasedOffset [
	<primitive: 634>
	^ self integerAt: zeroBasedOffset + 1 size: 2 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> int16AtOffset: zeroBasedOffset put: value [
	<primitive: 649>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 2 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> int32AtOffset: zeroBasedOffset [
	<primitive: 636>
	^ self integerAt: zeroBasedOffset + 1 size: 4 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> int32AtOffset: zeroBasedOffset put: value [
	<primitive: 651>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 4 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> int64AtOffset: zeroBasedOffset [
	<primitive: 638>
	^ self integerAt: zeroBasedOffset + 1 size: 8 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> int64AtOffset: zeroBasedOffset put: value [
	<primitive: 653>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 8 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> int8AtOffset: zeroBasedOffset [
	<primitive: 632>
	^ self integerAt: zeroBasedOffset + 1 size: 1 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> int8AtOffset: zeroBasedOffset put: value [
	<primitive: 647>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 1 signed: true
]

{ #category : 'accessing' }
ExternalAddress >> isExternalAddress [
	"Return true if the receiver describes the address of an object in the outside world"
	^true
]

{ #category : 'testing' }
ExternalAddress >> isNull [
	"Answer true if I am a null pointer"
	1 to: self size do:[:i| (self at: i) = 0 ifFalse:[^false]].
	^true
]

{ #category : 'accessing' }
ExternalAddress >> pointerAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self pointerAtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> pointerAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self pointerAtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> pointerAtOffset: zeroBasedOffset [
	<primitive: 639>
	"Answer a pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress new.
	1 to: ExternalAddress wordSize do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: zeroBasedOffset+i)].
	^addr
]

{ #category : 'accessing' }
ExternalAddress >> pointerAtOffset: zeroBasedOffset put: value [
	<primitive: 654>
	"Store a pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: ExternalAddress wordSize do:
		[:i|
		self unsignedByteAt: zeroBasedOffset+i put: (value basicAt: i)].
	^value
]

{ #category : 'printing' }
ExternalAddress >> printOn: aStream [
	"print this as a hex address ('@ 16rFFFFFFFF') to distinguish it from ByteArrays"

	aStream nextPutAll: '@ '; nextPutAll: (self asInteger storeStringBase: 16 length: 11 padded: true)
]

{ #category : 'copying' }
ExternalAddress >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	| dstAddress srcAddress repSize |

	self flag: #pharoTodo. "Maybe replace it with a primitive (like the one at Alien)?"
	repSize := stop - start + 1.
	dstAddress := start = 1
		ifTrue: [ self ]
		ifFalse: [ self + (start - 1) ].
	srcAddress := repStart = 1
		ifTrue: [ replacement ]
		ifFalse: [ replacement copyFrom: repStart to: repStart + repSize ].
	LibC memCopy: srcAddress to: dstAddress size: repSize
]

{ #category : 'accessing' }
ExternalAddress >> signedCharAt: byteOffset [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteOffset - 1
]

{ #category : 'accessing' }
ExternalAddress >> signedCharAt: byteIndex put: aCharacter [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteIndex - 1 put: aCharacter
]

{ #category : 'accessing' }
ExternalAddress >> signedLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int32AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> signedLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int32AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> signedLongLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int64AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> signedLongLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int64AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> signedShortAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int16AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> signedShortAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self int16AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> uint16AtOffset: zeroBasedOffset [
	<primitive: 633>
	^ self integerAt: zeroBasedOffset + 1 size: 2 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> uint16AtOffset: zeroBasedOffset put: value [
	<primitive: 648>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 2 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> uint32AtOffset: zeroBasedOffset [
	<primitive: 635>
	^ self integerAt: zeroBasedOffset + 1 size: 4 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> uint32AtOffset: zeroBasedOffset put: value [
	<primitive: 650>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 4 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> uint64AtOffset: zeroBasedOffset [
	<primitive: 637>
	^ self integerAt: zeroBasedOffset + 1 size: 8 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> uint64AtOffset: zeroBasedOffset put: value [
	<primitive: 652>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 8 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> uint8AtOffset: zeroBasedOffset [
	<primitive: 631>
	^ self integerAt: zeroBasedOffset + 1 size: 1 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> uint8AtOffset: zeroBasedOffset put: value [
	<primitive: 646>
	^ self integerAt: zeroBasedOffset + 1 put: value size: 1 signed: false
]

{ #category : 'accessing' }
ExternalAddress >> unsignedByteAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint8AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> unsignedByteAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint8AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> unsignedCharAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> unsignedCharAt: byteIndex put: aCharacter [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self char8AtOffset: byteIndex - 1 put: aCharacter
]

{ #category : 'accessing' }
ExternalAddress >> unsignedLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint32AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> unsignedLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint32AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> unsignedLongLongAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint64AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> unsignedLongLongAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint64AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> unsignedShortAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint16AtOffset: byteIndex - 1
]

{ #category : 'accessing' }
ExternalAddress >> unsignedShortAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."

	<ignoreMethodWithEquivalentInSuperclass>
	^ self uint16AtOffset: byteIndex - 1 put: value
]

{ #category : 'accessing' }
ExternalAddress >> utf8StringFromCString [

	"Assume that the receiver represents a C string and convert it to a byte array.
	WARNING: the referenced data MUST ends with a NULL character (byte 0).
	"

	self isNull ifTrue: [ ^ '' ].
	^ self bytesFromCString ifNotNil: [ :bytes | bytes utf8Decoded ]
]

{ #category : 'accessing' }
ExternalAddress >> value [
	^ self asInteger
]
