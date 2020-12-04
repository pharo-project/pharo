Extension { #name : #Alien }

{ #category : #'*UnifiedFFI' }
Alien >> asExternalAddress [

	^ ExternalAddress fromAddress: self address
]

{ #category : #'*UnifiedFFI' }
Alien >> boolean8AtOffset: zeroBasedOffset [
	^ (self unsignedByteAt: zeroBasedOffset + 1) ~= 0
]

{ #category : #'*UnifiedFFI' }
Alien >> boolean8AtOffset: zeroBasedOffset put: value [
	^ self unsignedByteAt: zeroBasedOffset + 1 put: (value ifTrue: [ 1 ] ifFalse: [ 0 ])
]

{ #category : #'*UnifiedFFI' }
Alien >> booleanAt: byteIndex [
	^ (self unsignedByteAt: byteIndex) ~= 0
]

{ #category : #'*UnifiedFFI' }
Alien >> booleanAt: byteIndex put: value [
	^ self unsignedByteAt: byteIndex put: (value ifTrue: [ 1 ] ifFalse: [ 0 ])
]

{ #category : #'*UnifiedFFI' }
Alien >> char16AtOffset: zeroBasedOffset [
	^ (self unsignedShortAt: zeroBasedOffset + 1) asCharacter
]

{ #category : #'*UnifiedFFI' }
Alien >> char16AtOffset: zeroBasedOffset put: value [
	^ self unsignedShortAt: zeroBasedOffset + 1 put: value asInteger
]

{ #category : #'*UnifiedFFI' }
Alien >> char32AtOffset: zeroBasedOffset [
	^ (self unsignedLongAt: zeroBasedOffset + 1) asCharacter
]

{ #category : #'*UnifiedFFI' }
Alien >> char32AtOffset: zeroBasedOffset put: value [
	^ self unsignedLongAt: zeroBasedOffset + 1 put: value asInteger
]

{ #category : #'*UnifiedFFI' }
Alien >> char8AtOffset: zeroBasedOffset [
	^ (self unsignedByteAt: zeroBasedOffset + 1) asCharacter
]

{ #category : #'*UnifiedFFI' }
Alien >> char8AtOffset: zeroBasedOffset put: value [
	^ self unsignedByteAt: zeroBasedOffset + 1 put: value asInteger
]

{ #category : #'*UnifiedFFI' }
Alien >> float32AtOffset: zeroBasedOffset [
	^ self floatAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> float32AtOffset: zeroBasedOffset put: value [
	^ self floatAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI' }
Alien >> float64AtOffset: zeroBasedOffset [
	^ self doubleAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> float64AtOffset: zeroBasedOffset put: value [
	^ self doubleAt: zeroBasedOffset + 1 put: value
]

{ #category : #'*UnifiedFFI' }
Alien >> int16AtOffset: zeroBasedOffset [
	^ self signedShortAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> int32AtOffset: zeroBasedOffset [
	^ self signedLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> int64AtOffset: zeroBasedOffset [
	^ self signedLongLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> int8AtOffset: zeroBasedOffset [
	^ self signedByteAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> integerAt: byteOffset size: size signed: signed [

	^ (ExternalAddress fromAddress: self address) integerAt: byteOffset size: size signed: signed
]

{ #category : #'*UnifiedFFI' }
Alien >> pointerAt: byteIndex [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."
	^ self pointerAtOffset: byteIndex - 1
]

{ #category : #'*UnifiedFFI' }
Alien >> pointerAt: byteIndex put: value [
	"This method is duplicated in this subclass with the purpose of ensuring a monomorphic inline cache in the following message send."
	^ self pointerAtOffset: byteIndex - 1 put: value
]

{ #category : #'*UnifiedFFI' }
Alien >> pointerAtOffset: zeroBasedOffset [
	"Answer a pointer object stored at the given byte address"
	| addr |
	addr := ExternalAddress new.
	1 to: ExternalAddress wordSize do:
		[:i|
		addr basicAt: i put: (self unsignedByteAt: zeroBasedOffset+i)].
	^addr
]

{ #category : #'*UnifiedFFI' }
Alien >> pointerAtOffset: zeroBasedOffset put: value [
	"Store a pointer object at the given byte address"
	value isExternalAddress ifFalse:
		[^self error:'Only external addresses can be stored'].
	1 to: ExternalAddress wordSize do:
		[:i|
		self unsignedByteAt: zeroBasedOffset+i put: (value basicAt: i)].
	^value
]

{ #category : #'*UnifiedFFI' }
Alien >> uint16AtOffset: zeroBasedOffset [
	^ self unsignedShortAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> uint32AtOffset: zeroBasedOffset [
	^ self unsignedLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> uint64AtOffset: zeroBasedOffset [
	^ self unsignedLongLongAt: zeroBasedOffset + 1
]

{ #category : #'*UnifiedFFI' }
Alien >> uint8AtOffset: zeroBasedOffset [
	^ self unsignedByteAt: zeroBasedOffset + 1
]
