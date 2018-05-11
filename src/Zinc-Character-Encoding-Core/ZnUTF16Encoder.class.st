"
I am ZnUTF16Encoder, a concrete subclass of ZnCharacterEncoder.
I implement the variable length UTF-16 encoding and decoding of Unicode according to RFC 2781.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-16

Part of Zinc HTTP Components.

"
Class {
	#name : #ZnUTF16Encoder,
	#superclass : #ZnEndianSensitiveUTFEncoder,
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnUTF16Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"

	^ self knownEncodingIdentifiers includes: (self canonicalEncodingIdentifier: string)
]

{ #category : #accessing }
ZnUTF16Encoder class >> knownEncodingIdentifiers [
	^ #( 'utf16' 'utf16be' 'utf16le' )
]

{ #category : #private }
ZnUTF16Encoder >> back16BitWordOnStream: stream [
	| firstByte secondByte |
	firstByte := stream back.
	secondByte := stream back.
	^ self isBigEndian 
		ifTrue: [ secondByte + (firstByte << 8) ]
		ifFalse: [ firstByte + (secondByte << 8) ]
]

{ #category : #converting }
ZnUTF16Encoder >> backOnStream: stream [
	"Move back one character on stream"

	| word |
	word := self back16BitWordOnStream: stream.
	(word < 16rD800 or: [ word > 16rDBFF ])
		ifFalse: [ 
			self back16BitWordOnStream: stream ]
]

{ #category : #converting }
ZnUTF16Encoder >> encodedByteCountForCodePoint: codePoint [
	"Return how many bytes are needed to encode integer code point"

	codePoint <= 65535 ifTrue: [ ^ 2 ].
	codePoint <= self maximumUTFCode ifTrue: [ ^ 4 ].
	self errorOutsideRange
]

{ #category : #'error handling' }
ZnUTF16Encoder >> errorIncomplete [
	self error: 'Incomplete utf-16 encoding'
]

{ #category : #accessing }
ZnUTF16Encoder >> identifier [
	^ #utf16
]

{ #category : #converting }
ZnUTF16Encoder >> nextCodePointFromStream: stream [
	"Read and return the next integer code point from stream"

	| word leadSurrogate trailSurrogate code |
	word := self read16BitWordFromStream: stream.
	(self processByteOrderMark: word)
		ifTrue: [ word := self read16BitWordFromStream: stream ].
	^ (word < 16rD800 or: [ word > 16rDBFF ])
		ifTrue: [
			word ]
		ifFalse: [ 
			leadSurrogate := word.
			trailSurrogate := self read16BitWordFromStream: stream.
			code := (leadSurrogate - 16rD800) * 16r400 + (trailSurrogate - 16rDC00).
			16r10000 + code ]
]

{ #category : #converting }
ZnUTF16Encoder >> nextPutCodePoint: codePoint toStream: stream [
	"Write the encoding for integer code point to stream"

	| leadSurrogate trailSurrogate shifted |
	(self isSurrogateCodePoint: codePoint) ifTrue: [ self errorOutsideRange ].
	codePoint <= 65535
		ifTrue: [ 
			^ self write16BitWord: codePoint toStream: stream ].
	codePoint <= self maximumUTFCode
		ifTrue: [
			shifted := codePoint - 16r10000.
			leadSurrogate := 16rD800 + (shifted // 16r400).
			trailSurrogate := 16rDC00 + (shifted \\ 16r400).
			self write16BitWord: leadSurrogate toStream: stream.
			self write16BitWord: trailSurrogate toStream: stream ]
		ifFalse: [
			self errorOutsideRange ]
]

{ #category : #private }
ZnUTF16Encoder >> processByteOrderMark: word [
	^ (word = 16rFEFF or: [ word = 16rFFFE ])
		ifTrue: [ 
			word = 16rFFFE
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]
]

{ #category : #private }
ZnUTF16Encoder >> read16BitWordFromStream: stream [
	| firstByte secondByte |
	firstByte := stream next.
	secondByte := stream next.
	(firstByte isNil or: [ secondByte isNil ])
		ifTrue: [ ^ self errorIncomplete ].
	^ self isBigEndian 
		ifTrue: [ secondByte + (firstByte << 8) ]
		ifFalse: [ firstByte + (secondByte << 8) ]
	
]

{ #category : #private }
ZnUTF16Encoder >> write16BitWord: word toStream: stream [
	self isBigEndian
		ifTrue: [ 
			stream
				nextPut: (word digitAt: 2);
				nextPut: (word digitAt: 1) ]
		ifFalse: [ 
			stream
				nextPut: (word digitAt: 1);
				nextPut: (word digitAt: 2) ]
]
