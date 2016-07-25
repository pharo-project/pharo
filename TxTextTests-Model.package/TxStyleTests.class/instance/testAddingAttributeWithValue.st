tests
testAddingAttributeWithValue

	| actual |
	style add: TxFontAttribute with: #font.
	
	actual := style at: TxFontAttribute. 

	self assert: (actual value == #font)