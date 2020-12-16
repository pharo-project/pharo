"
SUnit tests for ByteTextConverter
"
Class {
	#name : #ByteTextConverterTest,
	#superclass : #TestCase,
	#category : #'Multilingual-Tests-TextConversion'
}

{ #category : #testing }
ByteTextConverterTest >> testConversionToFrom [
	"Non regresson test for http://code.google.com/p/pharo/issues/detail?id=986"

	self assert: (('äöü' convertToEncoding: 'mac-roman') convertFromEncoding: 'mac-roman') equals: 'äöü'
]

{ #category : #testing }
ByteTextConverterTest >> testLatin2ToUnicodeConversion [
	| latin2Characters utfCodePoints decodingStream unicodeString |
	latin2Characters := #[16rBE 16rFD 16rE1 16rC8] asString.
	utfCodePoints := #(16r017E 16rFD 16rE1 16r010C).

	"Converters assume that characters you want to put/get from a stream are Unicode characters"
	"So our source is a string of latin 2"
	decodingStream := (MultiByteBinaryOrTextStream with: latin2Characters encoding: 'ISO-8859-2') reset.
	unicodeString := decodingStream contents.
	"our sourcelatin2 string contains characters outside latin1"
	self assert: unicodeString isWideString.
	self assert: (unicodeString at: 1) charCode equals: (utfCodePoints at: 1).
	self assert: (unicodeString at: 2) charCode equals: (utfCodePoints at: 2).
	self assert: (unicodeString at: 3) charCode equals: (utfCodePoints at: 3).
	self assert: (unicodeString at: 4) charCode equals: (utfCodePoints at: 4)
]

{ #category : #testing }
ByteTextConverterTest >> testMacRomanToUnicodeShorthand [
	| macRomanCharacter  unicodeCharacter   |
	macRomanCharacter := Character value: 16r80.
	unicodeCharacter := $Ä.

	self assert: macRomanCharacter macRomanToUnicode  equals: unicodeCharacter.
	self assert: macRomanCharacter equals: unicodeCharacter unicodeToMacRoman.
]

{ #category : #testing }
ByteTextConverterTest >> testUnicodeToLatin2Conversion [
	| latin2Bytes internalString encodingStream encodedBytes |
	latin2Bytes := #[16rBE 16rFD 16rE1 16rC8].
	internalString := String
		streamContents: [ :s | 
			s
				nextPut: 16r017E asCharacter;
				nextPut: 16rFD asCharacter;
				nextPut: 16rE1 asCharacter;
				nextPut: 16r010C asCharacter ].
	"Converters assume that characters you want to put/get from a stream are Unicode characters"
	"So we use a stream with an encoder, then put the internalString to it"
	encodingStream := (MultiByteBinaryOrTextStream with: String new encoding: 'ISO-8859-2') reset.
	encodingStream nextPutAll: internalString.
	"the encoded string in the stream does not contain any characters > 255"
	encodedBytes := encodingStream binary reset contents.


	self assert: (encodedBytes at: 1) equals: (latin2Bytes at: 1).
	self assert: (encodedBytes at: 2) equals: (latin2Bytes at: 2).
	self assert: (encodedBytes at: 3) equals: (latin2Bytes at: 3).
	self assert: (encodedBytes at: 4) equals: (latin2Bytes at: 4)
]
