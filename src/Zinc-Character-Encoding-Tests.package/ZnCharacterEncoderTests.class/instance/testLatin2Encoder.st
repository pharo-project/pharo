testing
testLatin2Encoder
	"Example characters taken from http://en.wikipedia.org/wiki/Latin2"
	
	| encoder inputBytes outputBytes inputString outputString |
	encoder := ZnCharacterEncoder newForEncoding: 'latin2'.
	inputString := String 
		with: (16r0154 asCharacter) with: (16r0110 asCharacter) 
		with: ( 16r0155 asCharacter) with: (16r0111 asCharacter).
	inputBytes := #(192 208 224 240) asByteArray.
	outputBytes := self encodeString: inputString with: encoder.
	self assert: outputBytes = inputBytes.
	outputString := self decodeBytes: inputBytes with: encoder.
	self assert: outputString = inputString 