as yet unclassified
testAddingSameStyleTwice
	| style attribute result |
	
	style := TxStyle new.
	attribute := TxForeColorAttribute with: Color red.
	style add: attribute.
	
	result := text addStyle: style.
	
	self assert: result == style