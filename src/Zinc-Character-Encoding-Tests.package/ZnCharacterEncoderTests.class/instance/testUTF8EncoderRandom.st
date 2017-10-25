testing
testUTF8EncoderRandom
	#(unicodeCharacterSource latin1CharacterSource asciiCharacterSource) do: [ :source |
		| string bytes result |
		string := self class stringOfSize: 256 fromSource: source.
		bytes := string utf8Encoded.
		result := bytes utf8Decoded.
		self assert: result equals: string ]