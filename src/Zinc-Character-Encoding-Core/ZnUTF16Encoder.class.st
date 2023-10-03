"
I am ZnUTF16Encoder, a concrete subclass of ZnCharacterEncoder.
I implement the variable length UTF-16 encoding and decoding of Unicode according to RFC 2781.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-16

Part of Zinc HTTP Components.

"
Class {
	#name : 'ZnUTF16Encoder',
	#superclass : 'ZnEndianSensitiveUTFEncoder',
	#category : 'Zinc-Character-Encoding-Core',
	#package : 'Zinc-Character-Encoding-Core'
}

{ #category : 'accessing' }
ZnUTF16Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"

	^ self knownEncodingIdentifiers includes: (self canonicalEncodingIdentifier: string)
]

{ #category : 'accessing' }
ZnUTF16Encoder class >> knownEncodingIdentifiers [
	^ #( 'utf16' 'utf16be' 'utf16le' )
]

{ #category : 'private' }
ZnUTF16Encoder >> back16BitWordOnStream: stream [
	| firstByte secondByte |
	firstByte := stream back.
	secondByte := stream back.
	^ self isBigEndian
		ifTrue: [ secondByte + (firstByte << 8) ]
		ifFalse: [ firstByte + (secondByte << 8) ]
]

{ #category : 'encoding - decoding' }
ZnUTF16Encoder >> backOnStream: stream [
	"Move back one character on stream"

	| word |
	word := self back16BitWordOnStream: stream.
	(self isSurrogateCodePoint: word)
		ifTrue: [
			self back16BitWordOnStream: stream ]
]

{ #category : 'encoding - decoding' }
ZnUTF16Encoder >> encodedByteCountForCodePoint: codePoint [
	"Return how many bytes are needed to encode integer code point"

	codePoint <= 65535 ifTrue: [ ^ 2 ].
	codePoint <= self maximumUTFCode ifTrue: [ ^ 4 ].
	self errorOutsideRange
]

{ #category : 'encoding - decoding' }
ZnUTF16Encoder >> ensureAtBeginOfCodePointOnStream: stream [
	"Ensure that the current position of stream is a the beginning of an encoded code point,
	if not move further backwards. This is necessary when a position in the binary stream is set,
	not knowing if that position is on a proper encoded character boundary."

	| word |
	"Each code point is encoded using words of 2 bytes, move backwards until our position is divisible by 2"
	[ stream position \\ 2 = 0 ] whileFalse: [ stream back ].
	"Now read the next word and back up"
	word := self read16BitWordFromStream: stream.
	self back16BitWordOnStream: stream.
	"If it is a low or trailing surrogate, move another word backwards until the high or leading surrogate"
	(word between: 16rDC00 and: 16rDFFF)
		ifTrue: [
			self back16BitWordOnStream: stream ]
]

{ #category : 'error handling' }
ZnUTF16Encoder >> errorIncomplete [
	self error: 'Incomplete utf-16 encoding'
]

{ #category : 'accessing' }
ZnUTF16Encoder >> identifier [
	^ #utf16
]

{ #category : 'testing' }
ZnUTF16Encoder >> isFixedLength [
	"Return true when I am a fixed length encoding"

	^ false
]

{ #category : 'encoding - decoding' }
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

{ #category : 'encoding - decoding' }
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

{ #category : 'private' }
ZnUTF16Encoder >> processByteOrderMark: word [
	^ (word = 16rFEFF or: [ word = 16rFFFE ])
		ifTrue: [
			word = 16rFFFE
				ifTrue: [ self swapEndianness ].
			true ]
		ifFalse: [ false ]
]

{ #category : 'private' }
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

{ #category : 'encoding - decoding' }
ZnUTF16Encoder >> skipToBeginOfCodePointOnStream: stream [
	"Ensure that the current position of stream is a the beginning of an encoded code point,
	if not move further forward. This is necessary when a position in the binary stream is set,
	not knowing if that position is on a proper encoded character boundary."

	| word |
	"Each code point is encoded using words of 2 bytes, move forward until our position is divisible by 2"
	[ stream position \\ 2 = 0 ] whileFalse: [ stream next ].
	"Now read the next word and back up"
	word := self read16BitWordFromStream: stream.
	self back16BitWordOnStream: stream.
	"If it is a low or trailing surrogate, move another word forward until the high or leading surrogate"
	(word between: 16rDC00 and: 16rDFFF)
		ifTrue: [
			self read16BitWordFromStream: stream ]
]

{ #category : 'private' }
ZnUTF16Encoder >> write16BitWord: word toStream: stream [
	self isBigEndian
		ifTrue: [
			stream
				nextPut: (word byteAt: 2);
				nextPut: (word byteAt: 1) ]
		ifFalse: [
			stream
				nextPut: (word byteAt: 1);
				nextPut: (word byteAt: 2) ]
]
