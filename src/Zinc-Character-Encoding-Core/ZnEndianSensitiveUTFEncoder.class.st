"
I am ZnEndianSensitiveUTFEncoder.
I am a ZnCharacterEncoder.
I add support for UTF encodings that are sensitive to endianness.
The default is big endian.
"
Class {
	#name : 'ZnEndianSensitiveUTFEncoder',
	#superclass : 'ZnUTFEncoder',
	#instVars : [
		'endianness'
	],
	#category : 'Zinc-Character-Encoding-Core',
	#package : 'Zinc-Character-Encoding-Core'
}

{ #category : 'instance creation' }
ZnEndianSensitiveUTFEncoder class >> newForEncoding: string [
	"Return a new character encoder object for an encoding described by string.
	Try to infer endianness from string, defaulting to big endian."

	| encoder |
	encoder := self new.
	(string asLowercase endsWith: 'be') ifTrue: [ encoder beBigEndian ].
	(string asLowercase endsWith: 'le') ifTrue: [ encoder beLittleEndian ].
	^ encoder
]

{ #category : 'comparing' }
ZnEndianSensitiveUTFEncoder >> = anObject [
	^ super = anObject and: [ self endianness == anObject endianness ]
]

{ #category : 'initialization' }
ZnEndianSensitiveUTFEncoder >> beBigEndian [
	endianness := #big
]

{ #category : 'initialization' }
ZnEndianSensitiveUTFEncoder >> beLittleEndian [
	endianness := #little
]

{ #category : 'accessing' }
ZnEndianSensitiveUTFEncoder >> endianness [
	^ endianness
]

{ #category : 'comparing' }
ZnEndianSensitiveUTFEncoder >> hash [
	^ super hash bitXor: self endianness hash
]

{ #category : 'initialization' }
ZnEndianSensitiveUTFEncoder >> initialize [
	endianness := #big
]

{ #category : 'testing' }
ZnEndianSensitiveUTFEncoder >> isBigEndian [
	^ endianness = #big
]

{ #category : 'testing' }
ZnEndianSensitiveUTFEncoder >> isLittleEndian [
	^ endianness = #little
]

{ #category : 'printing' }
ZnEndianSensitiveUTFEncoder >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(.
	stream nextPutAll: endianness; nextPutAll: ' endian'.
	stream nextPut: $)
]

{ #category : 'private' }
ZnEndianSensitiveUTFEncoder >> swapEndianness [
	self isLittleEndian
		ifTrue: [ self beBigEndian ]
		ifFalse: [ self beLittleEndian ]
]
