"
SUnit tests for UTF8TextConverter
"
Class {
	#name : #UTF8TextConverterTest,
	#superclass : #TestCase,
	#category : #'Multilingual-Tests-TextConversion'
}

{ #category : #testing }
UTF8TextConverterTest >> testNextPutAllStartingAt [
	"Test that non-ascii ByteString characters are converted correctly when using next:putAll:startingAt:"

	| converter stream source |
	converter := UTF8TextConverter new.
	stream := (String new: 10) writeStream.
	source := 'abcdefågh'.	"Notice å at 7, which is a non-ascii character in latin1-range, hence ByteString encoded"
	converter
		next: 5
		putAll: source
		startingAt: 5
		toStream: stream.
	"C3A5 is twobyte utf8-encoding of å"
	self assert: stream contents asByteArray equals: #[16r65 16r66 16rC3 16rA5 16r67 16r68]
]

{ #category : #testing }
UTF8TextConverterTest >> testPutSingle2ByteEncodedCharacter [
	| actual |
	"Test that the utf8-encoded bytes are stored when stream is ascii.
	(In the case of internal writeStreams, that its destination is a String)"
	actual := String
		streamContents: [ :stream | 
			| converter |
			converter := UTF8TextConverter new.
			converter nextPut: $ç toStream: stream ].
	self assert: actual asByteArray equals: #[2r11000011 2r10100111]
]

{ #category : #testing }
UTF8TextConverterTest >> testPutSingle3ByteEncodedCharacter [
	| actual |
	"Test that the utf8-encoded bytes are stored when stream is ascii.
	(In the case of internal writeStreams, that its destination is a String)"
	actual := String
		streamContents: [ :stream | 
			| converter |
			converter := UTF8TextConverter new.
			converter nextPut: (Unicode value: 16r20AC) toStream: stream ].
	self assert: actual asByteArray equals: #[2r11100010 2r10000010 2r10101100]
]

{ #category : #testing }
UTF8TextConverterTest >> testPutSingle4ByteEncodedCharacter [
	| actual |
	"Test that the utf8-encoded bytes are stored when stream is ascii.
	(In the case of internal writeStreams, that its destination is a String)"
	actual := String
		streamContents: [ :stream | 
			| converter |
			converter := UTF8TextConverter new.
			converter nextPut: (Unicode value: 16r24B62) toStream: stream ].
	self assert: actual asByteArray equals: #[2r11110000 2r10100100 2r10101101 2r10100010]
]

{ #category : #testing }
UTF8TextConverterTest >> testPutSingleCharacterBinary [
	| actual |
	"Test that the raw 8/32bit representations of Characters/WideCharacters are stored when stream is binary.
	(In the case of internal writeStreams, that its destination is a ByteArray)"
	actual := ByteArray
		streamContents: [ :stream | 
			| converter |
			converter := UTF8TextConverter new.
			converter nextPut: $a toStream: stream.
			converter nextPut: (Unicode value: 16r20AC) toStream: stream ].
	self assert: actual equals: #[97 0 0 16r20 16rAC]
]
