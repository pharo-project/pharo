testing - properties
testPropertyAtPut

	| testValue ivar |
	
	testValue := Date today.
	ivar := self class slotNamed: #ivarForTesting.
	
	ivar propertyAt: #testKeySelector put: testValue.
	self
		assert: (ivar propertyAt: #testKeySelector)
		equals: testValue.
	ivar removeProperty: #testKeySelector.
	self assert: ivar properties isNil.