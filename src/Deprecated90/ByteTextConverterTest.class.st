"
SUnit tests for ByteTextConverter
"
Class {
	#name : #ByteTextConverterTest,
	#superclass : #TestCase,
	#category : #Deprecated90
}

{ #category : #testing }
ByteTextConverterTest >> testConversionToFrom [
	"Non regresson test for http://code.google.com/p/pharo/issues/detail?id=986"

	self assert: (('äöü' convertToEncoding: 'mac-roman') convertFromEncoding: 'mac-roman') equals: 'äöü'
]

{ #category : #testing }
ByteTextConverterTest >> testLatin2ToUnicodeConversion [
	| latin2Characters utfCodePoints unicodeString |
	latin2Characters := #[16rBE 16rFD 16rE1 16rC8].
	utfCodePoints := #(16r017E 16rFD 16rE1 16r010C).

	"Converters assume that characters you want to put/get from a stream are Unicode characters"
	"So our source is a string of latin 2"
	unicodeString := (ZnCharacterEncoder newForEncoding: 'ISO-8859-2') decodeBytes: latin2Characters.

	"our sourcelatin2 string contains characters outside latin1"
	self assert: unicodeString isWideString.
	self assert: (unicodeString at: 1) charCode equals: (utfCodePoints at: 1).
	self assert: (unicodeString at: 2) charCode equals: (utfCodePoints at: 2).
	self assert: (unicodeString at: 3) charCode equals: (utfCodePoints at: 3).
	self assert: (unicodeString at: 4) charCode equals: (utfCodePoints at: 4)
]

{ #category : #testing }
ByteTextConverterTest >> testUnicodeToLatin2Conversion [
	| latin2Bytes internalString encodedBytes |
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
	encodedBytes := internalString encodeWith: 'ISO-8859-2'.

	self assert: (encodedBytes at: 1) equals: (latin2Bytes at: 1).
	self assert: (encodedBytes at: 2) equals: (latin2Bytes at: 2).
	self assert: (encodedBytes at: 3) equals: (latin2Bytes at: 3).
	self assert: (encodedBytes at: 4) equals: (latin2Bytes at: 4)
]
