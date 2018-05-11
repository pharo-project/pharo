"
I am ZnUTF32Encoder, a concrete subclass of ZnCharacterEncoder.
I implement the fixed length UTF-32 encoding and decoding of Unicode according to http://www.unicode.org/versions/Unicode8.0.0/ch03.pdf definitions D90, D99, D100 and D101.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-32

UCS-4 is another name for the same encoding.

Part of Zinc HTTP Components.

"
Class {
	#name : #ZnUTF32Encoder,
	#superclass : #ZnEndianSensitiveUTFEncoder,
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnUTF32Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"

	^ self knownEncodingIdentifiers includes: (self canonicalEncodingIdentifier: string)
]

{ #category : #accessing }
ZnUTF32Encoder class >> knownEncodingIdentifiers [
	^ #( 'utf32' 'utf32be' 'utf32le' 'ucs4' 'ucs4be' 'ucs4le')
]

{ #category : #converting }
ZnUTF32Encoder >> backOnStream: stream [
	"Move back one character on stream"

	4 timesRepeat: [ stream back ]
]

{ #category : #converting }
ZnUTF32Encoder >> encodedByteCountForCodePoint: codePoint [
	"Return how many bytes are needed to encode integer codePoint"

	codePoint > self maximumUTFCode 
		ifTrue: [ ^ self errorOutsideRange ].
	^ 4
]

{ #category : #'error handling' }
ZnUTF32Encoder >> errorIncomplete [
	self error: 'Incomplete utf-32 encoding'
]

{ #category : #accessing }
ZnUTF32Encoder >> identifier [
	^ #utf32
]

{ #category : #converting }
ZnUTF32Encoder >> nextCodePointFromStream: stream [
	"Read and return the next integer code point from stream"

	| codePoint |
	codePoint := self readCodePointFrom: stream.
	(self processByteOrderMark: codePoint)
		ifTrue: [ codePoint := self readCodePointFrom: stream ].
	((self isSurrogateCodePoint: codePoint) or: [ codePoint > self maximumUTFCode ]) 
		ifTrue: [ ^ self errorOutsideRange ].
	^ codePoint
]

{ #category : #converting }
ZnUTF32Encoder >> nextPutCodePoint: codePoint toStream: stream [
	"Write the encoding for integer code point to stream"

	(self isSurrogateCodePoint: codePoint) 
		ifTrue: [ self errorOutsideRange ].
	codePoint <= self maximumUTFCode
		ifTrue: [
			self writeCodePoint: codePoint to: stream ]
		ifFalse: [
			self errorOutsideRange ]
]

{ #category : #private }
ZnUTF32Encoder >> processByteOrderMark: codePoint [
	^ (codePoint = 16rFEFF or: [ codePoint = 16rFFFE0000 ])
		ifTrue: [ 
			codePoint = 16rFFFE0000
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]
]

{ #category : #private }
ZnUTF32Encoder >> readCodePointFrom: stream [
	| byte1 byte2 byte3 byte4 |
	byte1 := stream next.
	byte2 := stream next.
	byte3 := stream next.
	byte4 := stream next.
	(byte1 isNil or: [ byte2 isNil or: [ byte3 isNil or: [ byte4 isNil ] ] ])
		ifTrue: [ ^ self errorIncomplete ].
	^ self isBigEndian
		ifTrue: [ 
			(byte1 bitShift: 24) + (byte2 bitShift: 16) + (byte3 bitShift: 8) + byte4 ] 
		ifFalse: [ 
			(byte4 bitShift: 24) + (byte3 bitShift: 16) + (byte2 bitShift: 8) + byte1 ]
]

{ #category : #private }
ZnUTF32Encoder >> writeCodePoint: codePoint to: stream [
	self isBigEndian 
		ifTrue: [ 
			stream 
				nextPut: (codePoint digitAt: 4); 
				nextPut: (codePoint digitAt: 3); 
				nextPut: (codePoint digitAt: 2); 
				nextPut: (codePoint digitAt: 1) ]
		ifFalse: [ 
			stream 
				nextPut: (codePoint digitAt: 1); 
				nextPut: (codePoint digitAt: 2); 
				nextPut: (codePoint digitAt: 3); 
				nextPut: (codePoint digitAt: 4) ]
]
