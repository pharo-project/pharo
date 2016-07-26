properties
testPropertyBlockPersistent

	| testValue temp temp2 block |
	testValue := Date today.
	block := [ |t| t:=2].
	temp := block temporaryVariableNamed: #t.
	
	temp propertyAt: #testKeySelector put: testValue.
	self
		assert: (temp propertyAt: #testKeySelector)
		equals: testValue.
		
	temp2 := block temporaryVariableNamed: #t.
	
	self
		assert: (temp2 propertyAt: #testKeySelector)
		equals: testValue.
		
	self assert: temp2 == temp.
	self deny: block method savedTemps isNil.	
	temp removeProperty: #testKeySelector.
	self assert: temp properties isNil.
	
	self assert: block method savedTemps isNil.