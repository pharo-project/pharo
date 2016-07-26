test - properties
testPropertyValueAtPut
	| testValue |
	testValue := Date today.
	self class propertyAt: #testKeySelector put: testValue.
	self
		assert: (self class propertyAt: #testKeySelector)
		equals: testValue.
	self class removeProperty: #testKeySelector