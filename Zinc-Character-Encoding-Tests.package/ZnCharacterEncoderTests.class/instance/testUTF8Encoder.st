testing
testUTF8Encoder
	"The examples are taken from http://en.wikipedia.org/wiki/UTF-8#Description"
	
	| encoder inputBytes outputBytes inputString outputString |
	encoder := ZnUTF8Encoder new.
	inputString := String with: $$ with: (16r00A2 asCharacter) with: (16r20AC asCharacter) with: (16r024B62 asCharacter).
	inputBytes := #(16r24 16rC2 16rA2 16rE2 16r82 16rAC 16rF0 16rA4 16rAD 16rA2) asByteArray.
	outputBytes := self encodeString: inputString with: encoder.
	self assert: outputBytes = inputBytes.
	outputString := self decodeBytes: inputBytes with: encoder.
	self assert: outputString = inputString 