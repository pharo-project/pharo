tests
testGettingAbsentAttributeValue

	| actual |
	
	style removeAll.
	
	actual := style at: TxFontAttribute ifAbsent: [#result].
	
	self assert: actual == #result