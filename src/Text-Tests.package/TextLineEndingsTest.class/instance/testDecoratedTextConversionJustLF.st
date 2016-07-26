tests
testDecoratedTextConversionJustLF

	| text newText |
	text := ('123456', String lf, '901234') asText.
	text addAttribute: TextColor blue from: 4 to: 10.
	text addAttribute: TextColor red from: 6 to: 9.
	text addAttribute: TextEmphasis bold.
	newText := text withSqueakLineEndings.
	self assert: ((text size) = newText size).
	self assert: (newText size = newText runs size).
	self assert: (newText attributesAt: 6) = (text attributesAt: 6).
	self assert: (newText attributesAt: 8) = (text attributesAt: 8).