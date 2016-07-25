testing
testCodePointEncodingDecoding
	| encoder input output |
	input := 'Düsseldorf Königsallee' collect: #codePoint as: Array.
	self assert: input isCollection.
	self assert: (input allSatisfy: #isInteger).
	#(utf8 utf16 utf32 latin1 null) do: [ :each |
		encoder := each asZnCharacterEncoder.
		output := encoder encodeCodePoints: input.
		self assert: output isCollection.
		self assert: (output allSatisfy: [ :e | e isInteger and: [ e between: 0 and: 255 ] ] ).
		self assert: (encoder encodedByteCountForCodePoints: input) equals: output size.
		self assert: (encoder decodeAsCodePoints: output) equals: input ]