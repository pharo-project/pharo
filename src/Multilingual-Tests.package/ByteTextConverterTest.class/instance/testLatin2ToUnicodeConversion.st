testing
testLatin2ToUnicodeConversion
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