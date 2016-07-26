tests  - properties
testPropertyAtPut

	| testValue |
	
	testValue := Date today.
	self class binding propertyAt: #testKeySelector put: testValue.
	self
		assert: (self class binding propertyAt: #testKeySelector)
		equals: testValue.
	self class binding removeProperty: #testKeySelector.
	self assert: self class binding properties isNil.