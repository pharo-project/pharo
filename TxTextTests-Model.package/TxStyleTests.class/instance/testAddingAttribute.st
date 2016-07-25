tests
testAddingAttribute

	| attribute |
	attribute := TxFontAttribute new.
	
	style add: attribute.
	
	self assert: (style includes: attribute)