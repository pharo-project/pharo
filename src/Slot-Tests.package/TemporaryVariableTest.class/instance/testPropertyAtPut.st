properties
testPropertyAtPut

	| testValue temp |
	
	testValue := Date today.
	temp := thisContext temporaryVariableNamed:  #testValue.
	
	temp propertyAt: #testKeySelector put: testValue.
	self
		assert: (temp propertyAt: #testKeySelector)
		equals: testValue.
		
	temp removeProperty: #testKeySelector.
	self assert: temp properties isNil.