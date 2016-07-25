helpers
assert: originalText isTheSameAfterLittleEndianConversion: aBooleanOrNil

	"Test that encoding / decoding a string witll produce the original string for both byte orders"

	| converter bytes decodedText |

	converter := UTF16TextConverter new.
	aBooleanOrNil ifNotNil: [converter useLittleEndian: aBooleanOrNil].
	bytes := (originalText convertToWithConverter: converter).
	decodedText := bytes convertFromWithConverter: converter.
	
	self assert: originalText equals: decodedText.
	