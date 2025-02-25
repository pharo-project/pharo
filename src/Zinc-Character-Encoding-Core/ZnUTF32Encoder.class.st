"
I am ZnUTF32Encoder, a concrete subclass of ZnCharacterEncoder.
I implement the fixed length UTF-32 encoding and decoding of Unicode according to http://www.unicode.org/versions/Unicode8.0.0/ch03.pdf definitions D90, D99, D100 and D101.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-32

UCS-4 is another name for the same encoding.

Part of Zinc HTTP Components.

"
Class {
	#name : 'ZnUTF32Encoder',
	#superclass : 'ZnEndianSensitiveUTFEncoder',
	#category : 'Zinc-Character-Encoding-Core',
	#package : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnUTF32Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"

	^ self knownEncodingIdentifiers includes: (self canonicalEncodingIdentifier: string)
]

{ #category : 'accessing' }
ZnUTF32Encoder class >> knownEncodingIdentifiers [
	^ #( 'utf32' 'utf32be' 'utf32le' 'ucs4' 'ucs4be' 'ucs4le')
]

{ #category : 'encoding - decoding' }
ZnUTF32Encoder >> backOnStream: stream [
	"Move back one character on stream"

	4 timesRepeat: [ stream back ]
]

{ #category : 'encoding - decoding' }
ZnUTF32Encoder >> encodedByteCountForCodePoint: codePoint [
	"Return how many bytes are needed to encode integer codePoint"

	codePoint > self maximumUTFCode
		ifTrue: [ ^ self errorOutsideRange ].
	^ 4
]

{ #category : 'encoding - decoding' }
ZnUTF32Encoder >> ensureAtBeginOfCodePointOnStream: stream [
	"Ensure that the current position of stream is a the beginning of an encoded code point,
	if not move further backwards. This is necessary when a position in the binary stream is set,
	not knowing if that position is on a proper encoded character boundary."

	"Each code point is encoded using exaxtly 4 bytes, move backwards until our position is divisible by 4"

	[ stream position \\ 4 = 0 ] whileFalse: [ stream back ]
]

{ #category : 'error handling' }
ZnUTF32Encoder >> errorIncomplete [
	self error: 'Incomplete utf-32 encoding'
]

{ #category : 'accessing' }
ZnUTF32Encoder >> identifier [
	^ #utf32
]

{ #category : 'encoding - decoding' }
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

{ #category : 'encoding - decoding' }
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

{ #category : 'private' }
ZnUTF32Encoder >> processByteOrderMark: codePoint [
	^ (codePoint = 16rFEFF or: [ codePoint = 16rFFFE0000 ])
		ifTrue: [
			codePoint = 16rFFFE0000
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]
]

{ #category : 'private' }
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

{ #category : 'encoding - decoding' }
ZnUTF32Encoder >> skipToBeginOfCodePointOnStream: stream [
	"Ensure that the current position of stream is a the beginning of an encoded code point,
	if not move further forward. This is necessary when a position in the binary stream is set,
	not knowing if that position is on a proper encoded character boundary."

	"Each code point is encoded using exaxtly 4 bytes, move forward until our position is divisible by 4"

	[ stream position \\ 4 = 0 ] whileFalse: [ stream next ]
]

{ #category : 'private' }
ZnUTF32Encoder >> writeCodePoint: codePoint to: stream [
	self isBigEndian
		ifTrue: [
			stream
				nextPut: (codePoint byteAt: 4);
				nextPut: (codePoint byteAt: 3);
				nextPut: (codePoint byteAt: 2);
				nextPut: (codePoint byteAt: 1) ]
		ifFalse: [
			stream
				nextPut: (codePoint byteAt: 1);
				nextPut: (codePoint byteAt: 2);
				nextPut: (codePoint byteAt: 3);
				nextPut: (codePoint byteAt: 4) ]
]
