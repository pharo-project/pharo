test-attribute
testApplyFontMultipleLine
	"When applying same attribute over whole text, all spans should have it"
	
	| font |
	self newText: String cr,'AAA',String cr,'CCC',String cr,'ttt',String cr.
	font := TxFontAttribute new.
	selection all.
	selection applyAttribute: font.

	text spans do: [:span |
		self assert: (span style includes: font) ]
