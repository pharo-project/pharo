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

	self assert: (('äöü' convertToEncoding: 'mac-roman') convertFromEncoding: 'mac-roman') = 'äöü'
]

{ #category : #testing }
ByteTextConverterTest >> testLatin2ToUnicodeConversion [
        | latin2Characters utfCodePoints decodingStream unicodeString |

        latin2Characters := #[16rBE 16rFD 16rE1 16rC8] asString.
	utfCodePoints := #(16r017E 16rFD 16rE1 16r010C).
	
	"Converters assume that characters you want to put/get from a stream are Unicode characters"
	"So our source is a string of latin 2"
	decodingStream := (MultiByteBinaryOrTextStream 
                with:  latin2Characters 
                encoding: 'ISO-8859-2') reset.
	unicodeString := decodingStream contents.
	"our sourcelatin2 string contains characters outside latin1"
	self assert: unicodeString isWideString.
	self assert: (unicodeString at: 1) charCode = (utfCodePoints at: 1) .
	self assert: (unicodeString at: 2) charCode = (utfCodePoints at: 2) .
	self assert: (unicodeString at: 3) charCode = (utfCodePoints at: 3) .
	self assert: (unicodeString at: 4) charCode = (utfCodePoints at: 4) .
]

{ #category : #testing }
ByteTextConverterTest >> testLineEndConversion [
	"Regression test for line end conversion.
	Many byte encodings have overlap in the non-ascii range (and thus 0 entries in the 128-255 range of latin1Map, meaning they shouldn't be converted.
	Previously a full nonAsciiMap with 1 as entries for the entire 128-255 range was installed.
	This only ever really worked for utf-8 - for byte text converters it would lead to exceptions on any non-ascii character that didn't have to be converted."
	| cp1252Bytes internalString converter encodedBytes  |
        
	"æøå all map to the same code points in cp1252 as in unicode."
	cp1252Bytes := #[16rE6 16r0D 16r0A 16rF8 16r0D 16r0A 16rE5] .
	internalString := String streamContents: [:s | 
		s nextPut: $æ;
			nextPut: Character cr;
			nextPut: $ø;
			nextPut: Character cr;
			nextPut: $å].
	converter := CP1252TextConverter new.
	converter installLineEndConvention: String crlf.
	
	encodedBytes := (converter convertToSystemString: internalString) asByteArray.
	
	encodedBytes with: cp1252Bytes do: [ :encoded :truth | 
		self assert: encoded equals: truth].	
]

{ #category : #testing }
ByteTextConverterTest >> testLineEndConversionWideString [
	"Regression test for line end conversion of wide strings.
	- This uses a different path from the optimized path employed for byte (latin1) strings,
	and essentially translates character by character.
	This was written before line end conversion was added to the converters, assumed all translations were 1-1, and didn't use the latin1Encodings where line end conversion is installed at all, so never worked..."
	| cp1252Bytes internalString converter encodedBytes  |
        
	"æøå all map to the same code points in cp1252 as in unicode. 
	€ however, is codePoint 16r20AC in unicode (meaning the internalString will be wide), 
	but 16r80 in cp1252"
	
	cp1252Bytes := #[16rE6 16r0D 16r0A 16rF8 16r0D 16r0A 16rE5 16r0D 16r0A 16r80] .
	internalString := String streamContents: [:s | 
		s nextPut: $æ;
			nextPut: Character cr;
			nextPut: $ø;
			nextPut: Character cr;
			nextPut: $å;
			nextPutAll: String cr;
			nextPut: $€].
	converter := CP1252TextConverter new.
	converter installLineEndConvention: String crlf.
	
	encodedBytes := (converter convertToSystemString: internalString) asByteArray.
	
	encodedBytes with: cp1252Bytes do: [ :encoded :truth | 
		self assert: encoded equals: truth].	
]

{ #category : #testing }
ByteTextConverterTest >> testUnicodeToLatin2Conversion [
        | latin2Bytes internalString encodingStream encodedBytes  |

        latin2Bytes := #[16rBE 16rFD 16rE1 16rC8] .
	internalString := String streamContents: [:s | 
		s nextPut: 16r017E asCharacter;
		   nextPut: 16rFD asCharacter;
		  nextPut: 16rE1 asCharacter;
		  nextPut: 16r010C asCharacter].
	"Converters assume that characters you want to put/get from a stream are Unicode characters"
	"So we use a stream with an encoder, then put the internalString to it"
	encodingStream := (MultiByteBinaryOrTextStream 
                with:  String new 
                encoding: 'ISO-8859-2') reset.
	encodingStream nextPutAll: internalString.
	"the encoded string in the stream does not contain any characters > 255"
	encodedBytes := encodingStream binary reset contents.
	
	
	self assert: (encodedBytes at: 1)  = (latin2Bytes at: 1) .
	self assert: (encodedBytes at: 2)  = (latin2Bytes at: 2) .
	self assert: (encodedBytes at: 3)  = (latin2Bytes at: 3) .
	self assert: (encodedBytes at: 4)  = (latin2Bytes at: 4) .
]
