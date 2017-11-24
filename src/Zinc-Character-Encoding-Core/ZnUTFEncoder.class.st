"
I am ZnUTFEncoder. I am a ZnCharacterEncoder. My subclasses deal with the full range of Unicode character code points.
"
Class {
	#name : #ZnUTFEncoder,
	#superclass : #ZnCharacterEncoder,
	#category : #'Zinc-Character-Encoding-Core'
}

{ #category : #accessing }
ZnUTFEncoder class >> handlesEncoding: string [
	"Return true when my instances handle the encoding described by string"
	
	^ false
]

{ #category : #accessing }
ZnUTFEncoder >> byteOrderMark [
	"The code point of the Unicode Byte-Order-Mark or BOM character.
	See https://en.wikipedia.org/wiki/Byte_order_mark"
	
	^ 16rFEFF
]

{ #category : #convenience }
ZnUTFEncoder >> decodeBytes: bytes [
	"Overridden to prevent the automagic switch from ByteString to WideString 
	and the slow #becomeForward: in there. See also #decodeBytesIntoWideString:"
	
	| byteStream |
	byteStream := bytes readStream.
	^ String streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [ | codePoint |
			codePoint := self nextCodePointFromStream: byteStream.
			(codePoint > 255 and: [ stream originalContents isWideString not ])
				ifTrue: [ | wideString position |
					position := stream position.
					wideString := WideString from: stream originalContents.
					stream on: wideString; setFrom: position + 1 to: position ].
			stream nextPut: (Character value: codePoint) ] ] 
]

{ #category : #convenience }
ZnUTFEncoder >> decodeBytesIntoWideString: bytes [
	"Variant of #decodeBytes: that is faster when you know upfront that a WideString is probably needed"

	| byteStream |
	byteStream := bytes readStream.
	^ WideString streamContents: [ :stream |
		[ byteStream atEnd ] whileFalse: [
			stream nextPut: (self nextFromStream: byteStream) ] ] 
]

{ #category : #convenience }
ZnUTFEncoder >> encodeStringWithByteOrderMark: string [
	"Encode string and return the resulting byte array.
	Always add a Unicode byte order mark (BOM) in front."
	
	^ ByteArray streamContents: [ :stream |
		self nextPutByteOrderMarkToStream: stream.
		self next: string size putAll: string startingAt: 1 toStream: stream ]
]

{ #category : #testing }
ZnUTFEncoder >> isSurrogateCodePoint: codePoint [
	"Surrogate Code Points should not be encoded or decoded because they are not Unicode scalar values"
	
	^ codePoint between: 16rD800 and: 16rDFFF
]

{ #category : #accessing }
ZnUTFEncoder >> maximumUTFCode [
	^ 16r10FFFF
]

{ #category : #convenience }
ZnUTFEncoder >> nextPutByteOrderMarkToStream: stream [
	"Write the encoded byte-order-mark (BOM) to stream"
	
	self nextPutCodePoint: self byteOrderMark toStream: stream 
]
