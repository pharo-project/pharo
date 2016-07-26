testing
testCodePointStreams
	| string codePoints bytes result |
	string := 'Düsseldorf Königsallee'.
	codePoints := string collect: #codePoint as: Array.
	self assert: codePoints isCollection.
	self assert: (codePoints allSatisfy: #isInteger).
	#(utf8 utf16 utf32 latin1 null) do: [ :each |
		bytes := ByteArray streamContents: [ :out |
			(ZnCodePointWriteStream on: out encoding: each)
				nextPutAll: codePoints ].
		self assert: bytes equals: (each asZnCharacterEncoder encodeString: string).
		result := (ZnCodePointReadStream on: bytes readStream encoding: each) upToEnd.
		self assert: result equals: codePoints.
		self assert: (codePoints collect: #asCharacter as: String) equals: string ]