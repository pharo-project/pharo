tests  - properties
testPropertyAtPut

	| testValue classVariable |
	
	testValue := Date today.
	classVariable := self class classVariableNamed: #TestVariable.
	
	classVariable propertyAt: #testKeySelector put: testValue.
	self
		assert: (classVariable propertyAt: #testKeySelector)
		equals: testValue.

	TestVariable := 5. "of course changing the var does not change the property"
	
	self
		assert: (classVariable propertyAt: #testKeySelector)
		equals: testValue.
		
	classVariable removeProperty: #testKeySelector.
	self assert: classVariable properties isNil.