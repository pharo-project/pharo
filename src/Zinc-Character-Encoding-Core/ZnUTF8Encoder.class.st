"
I am ZnUTF8Encoder, a concrete subclass of ZnCharacterEncoder.
I implement the variable length UTF-8 encoding and decoding of Unicode according to RFC 3629.

Wikipedia reference http://en.wikipedia.org/wiki/UTF-8

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnUTF8Encoder,
	#superclass : #ZnUTFEncoder,
	#classVars : [
		'ByteASCIISet',
		'ByteUTF8Encoding',
		'Default'
	],
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnUTF8Encoder class >> default [
	"Return a cached instance of the most commonly used encoder,
	which is faster than going via #newForEncoding: that does a subclass search"
	
	^ Default ifNil: [ Default := self new ]
]

{ #category : #accessing }
ZnUTF8Encoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"
	
	^ (self canonicalEncodingIdentifier: string) = 'utf8'
]

{ #category : #'class initialization' }
ZnUTF8Encoder class >> initialize [
	| encoder stream |
	ByteASCIISet := ByteArray new: 256.
	ByteUTF8Encoding := Array new: 256.
	encoder := self new.
	stream := ByteArray new writeStream.
	0 to: 255 do: [ :each |
		| bytes |
		stream reset.
		encoder nextPut: (Character value: each) toStream: stream.
		bytes := stream contents.
		(bytes size = 1 and: [ bytes first = each ])
			ifTrue: [
				ByteASCIISet at: each + 1 put: 0 ]
			ifFalse: [  
				ByteASCIISet at: each + 1 put: 1.
				ByteUTF8Encoding at: each + 1 put: bytes ] ] 
]

{ #category : #accessing }
ZnUTF8Encoder class >> knownEncodingIdentifiers [
	^ #( utf8 )
]

{ #category : #'instance creation' }
ZnUTF8Encoder class >> newForEncoding: string [
	"No further parametrization needed"
	
	^ self new
]

{ #category : #converting }
ZnUTF8Encoder >> backOnStream: stream [
	"Move back one character on stream"

	[ (stream back bitAnd: 2r11000000) == 2r10000000 ] whileTrue
]

{ #category : #converting }
ZnUTF8Encoder >> encodedByteCountForCodePoint: codePoint [
	"Return how many bytes are needed to encode integer code point"

	codePoint < 128 ifTrue: [ ^ 1 ].
	codePoint < 2048 ifTrue: [ ^ 2 ].
	codePoint < 65535 ifTrue: [ ^ 3 ].
	codePoint <= self maximumUTFCode ifTrue: [ ^ 4 ].
	self errorOutsideRange.
	^ 0
]

{ #category : #'error handling' }
ZnUTF8Encoder >> error: message [
	^ ZnInvalidUTF8 signal: message
]

{ #category : #'error handling' }
ZnUTF8Encoder >> errorIllegalContinuationByte [
	^ self error: 'Illegal continuation byte for utf-8 encoding'
]

{ #category : #'error handling' }
ZnUTF8Encoder >> errorIllegalLeadingByte [
	^ self error: 'Illegal leading byte for utf-8 encoding'
]

{ #category : #'error handling' }
ZnUTF8Encoder >> errorOverlong [
	^ self error: 'Overlong utf-8 encoding (non-shortest form)'
]

{ #category : #private }
ZnUTF8Encoder >> findFirstNonASCIIIn: string startingAt: offset [
	"This calls a fast primitive. Note that string can be a ByteString or ByteArray"
	
	offset > string size ifTrue: [ ^ 0 ].
	^ ByteString 
		findFirstInString: string 
		inSet: ByteASCIISet 
		startingAt: offset
]

{ #category : #accessing }
ZnUTF8Encoder >> identifier [
	^ #utf8
]

{ #category : #convenience }
ZnUTF8Encoder >> next: count putAll: string startingAt: offset toStream: stream [
	"Write count characters from string starting at offset to stream."
	"Overwritten for performance reasons - create a fast path for byte strings"
	
	string isByteString
		ifTrue: [ self next: count putAllByteString: string startingAt: offset toStream: stream ]
		ifFalse: [ super next: count putAll: string startingAt: offset toStream: stream ]
]

{ #category : #private }
ZnUTF8Encoder >> next: count putAllASCII: string startingAt: offset toStream: stream [
	"Write count bytes from string starting at offset to stream,
	assuming all characters are in the ASCII set and need no translation"
	
	offset to: offset + count - 1 do: [ :index | 
		stream nextPut: (string byteAt: index) ]
]

{ #category : #private }
ZnUTF8Encoder >> next: count putAllByteString: string startingAt: offset toStream: stream [
	"A faster version when string is a ByteString"
	
	| lastIndex nextIndex |
	lastIndex := offset.
	nextIndex := self findFirstNonASCIIIn: string startingAt: lastIndex.
	(nextIndex = 0 or: [ offset + count <= nextIndex ]) 
		ifTrue: [ 
			^ self next: count putAllASCII: string startingAt: offset toStream: stream ].
	[	nextIndex > lastIndex 
			ifTrue: [
				self next: nextIndex - lastIndex putAllASCII: string startingAt: lastIndex toStream: stream ].
		stream nextPutAll: (ByteUTF8Encoding at: (string byteAt: nextIndex) + 1).
		lastIndex := nextIndex + 1.
		nextIndex := self findFirstNonASCIIIn: string startingAt: lastIndex.
		nextIndex = 0 or: [ offset + count <= nextIndex ] ] whileFalse.
	offset + count <= lastIndex 
		ifFalse: [ 
			self next: offset + count - lastIndex putAllASCII: string startingAt: lastIndex toStream: stream ]
	
]

{ #category : #converting }
ZnUTF8Encoder >> nextCodePointFromStream: stream [
	"Read and return the next integer code point from stream"

	| code byte next |
	(byte := stream next ifNil: [ ^ self errorIncomplete ]) < 128
		ifTrue: [ ^ byte ].
	(byte bitAnd: 2r11100000) == 2r11000000
		ifTrue: [ 
			code := byte bitAnd: 2r00011111.
			((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
				ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
				ifFalse: [ ^ self errorIllegalContinuationByte ].
			code < 128 ifTrue: [ ^ self errorOverlong ].
			^ code ].
	(byte bitAnd: 2r11110000) == 2r11100000
		ifTrue: [ 
			code := byte bitAnd: 2r00001111.
			2 timesRepeat: [ 
				((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
					ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
					ifFalse: [ ^ self errorIllegalContinuationByte ] ].
			code < 2048 ifTrue: [ ^ self errorOverlong ].
			(self isSurrogateCodePoint: code) ifTrue: [ ^ self errorOutsideRange ].
			code = 65279 "Unicode Byte Order Mark" ifTrue: [ 
				stream atEnd ifTrue: [ ^ self errorIncomplete ].
				^ self nextCodePointFromStream: stream ].
			^ code ].
	(byte bitAnd: 2r11111000) == 2r11110000
		ifTrue: [ 
			code := byte bitAnd: 2r00000111.
			3 timesRepeat: [ 
				((next := stream next ifNil: [ ^ self errorIncomplete ]) bitAnd: 2r11000000) == 2r10000000
					ifTrue: [ code := (code bitShift: 6) + (next bitAnd: 2r00111111) ]
					ifFalse: [ ^ self errorIllegalContinuationByte ] ].
			code < 65535 ifTrue: [ ^ self errorOverlong ].
			^ code ].
	^ self errorIllegalLeadingByte
]

{ #category : #converting }
ZnUTF8Encoder >> nextPutCodePoint: codePoint toStream: stream [
	"Write the encoding for Integer code point to stream"

	codePoint < 128 ifTrue: [ 
		^ stream nextPut: codePoint ].
	codePoint < 2048 ifTrue: [ 
		^ stream 
			nextPut: (2r11000000 + (codePoint bitShift: -6)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	(self isSurrogateCodePoint: codePoint) ifTrue: [ ^ self errorOutsideRange ].
	codePoint < 65536 ifTrue: [ 
		^ stream 
			nextPut: (2r11100000 + (codePoint bitShift: -12));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	codePoint <= self maximumUTFCode ifTrue: [ 
		^ stream 
			nextPut: (2r11110000 + (codePoint bitShift: -18));
			nextPut: (2r10000000 + ((codePoint bitShift: -12) bitAnd: 2r111111));
			nextPut: (2r10000000 + ((codePoint bitShift: -6) bitAnd: 2r111111)); 
			nextPut: (2r10000000 + (codePoint bitAnd: 2r111111)) ].
	^ self errorOutsideRange

]

{ #category : #convenience }
ZnUTF8Encoder >> readInto: string startingAt: offset count: requestedCount fromStream: stream [
	"Read requestedCount characters into string starting at offset,
	returning the number read, there could be less available when stream is atEnd.
	I signal a ZnByteStringBecameWideString notification if necessary"

	| stringBuffer |
	stringBuffer := string.
	offset to: offset + requestedCount - 1 do: [ :index | | codePoint |
		stream atEnd ifTrue: [ ^ index - offset ].  
		codePoint := self nextCodePointFromStream: stream.
		(codePoint > 255 and: [ stringBuffer isWideString not ])
			ifTrue: [ stringBuffer := ZnByteStringBecameWideString convert: stringBuffer ].
		stringBuffer at: index put: (Character value: codePoint) ].
	^ requestedCount
]
