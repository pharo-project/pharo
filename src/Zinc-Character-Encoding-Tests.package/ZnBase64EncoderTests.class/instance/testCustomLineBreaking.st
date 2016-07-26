testing
testCustomLineBreaking
	| encoder input output charCount |
	encoder := ZnBase64Encoder new.
	encoder breakLinesAt: 16.
	input := (0 to: 255) asByteArray.
	output := encoder encode: input.
	self assert: (encoder decode: output) equals: input.
	charCount := ((256 // 3) + (256 \\ 3) sign) * 4.
	self assert: output size equals: (charCount + (charCount // 16 * String crlf size))