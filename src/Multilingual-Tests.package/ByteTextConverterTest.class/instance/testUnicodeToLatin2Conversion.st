testing
testUnicodeToLatin2Conversion
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