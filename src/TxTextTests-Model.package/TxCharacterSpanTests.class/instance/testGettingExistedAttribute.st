tests
testGettingExistedAttribute

	|  actual |
	
	span style add: TxFontAttribute with: #font.
	
	actual := span getAttribute: TxFontAttribute.
	
	self assert: actual == #font
	