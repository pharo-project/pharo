as yet unclassified
testAddingCopyOfSameStyleReturnsOriginalStyle
	| style attribute result original copy |
	
	style := TxStyle new.
	attribute := TxForeColorAttribute with: Color red.
	style add: attribute.
	
	original := text addStyle: style.
	copy := original copy.
		
	result := text addStyle: copy.
	
	self assert: result == original