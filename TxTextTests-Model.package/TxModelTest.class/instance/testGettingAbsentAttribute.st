tests-attributes accessing
testGettingAbsentAttribute

	|  actual attributeClass |
	
	attributeClass := TxMockAttribute.
	actual := text defaultStyle get: attributeClass.
	
	self assert: actual equals: attributeClass defaultValue 