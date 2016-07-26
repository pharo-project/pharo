tests-attributes accessing
testGettingExistedAttribute

	| actual |
		
	text defaultStyle add: TxFontAttribute with: #font.
	
	actual := text defaultStyle get: TxFontAttribute.
	
	self assert: actual == #font
	