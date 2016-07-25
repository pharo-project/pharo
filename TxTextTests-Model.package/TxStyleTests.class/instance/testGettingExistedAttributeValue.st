tests
testGettingExistedAttributeValue

	| attribute actual |
	attribute := TxFontAttribute with: #font.
	
	style add: attribute.
	
	actual := style get: TxFontAttribute ifAbsent: nil.
	
	self assert: actual == #font