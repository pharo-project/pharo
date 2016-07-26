testing
testFullAlphabet
	| encoder input output |
	encoder := ZnBase64Encoder new.
	input := encoder alphabet.
	output := encoder decode: input.
	self assert: (encoder encode: output) equals: input.
	encoder breakLines.
	output := encoder decode: input.
	self assert: (encoder encode: output) equals: input