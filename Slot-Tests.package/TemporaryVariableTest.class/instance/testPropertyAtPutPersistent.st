properties
testPropertyAtPutPersistent

	| testValue temp temp2 |
	testValue := Date today.
	temp := thisContext method temporaryVariableNamed: #temp.
	
	temp propertyAt: #testKeySelector put: testValue.
	self
		assert: (temp propertyAt: #testKeySelector)
		equals: testValue.
		
	temp2 := thisContext method temporaryVariableNamed: #temp.
	
	self
		assert: (temp2 propertyAt: #testKeySelector)
		equals: testValue.
		
	self assert: temp2 == temp.
	self deny: thisContext method savedTemps isNil.	
	temp removeProperty: #testKeySelector.
	self assert: temp properties isNil.
	
	self assert: thisContext method savedTemps isNil.