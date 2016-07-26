testing - properties
testRemoveProperty
	| ivar | 
	ivar := self class slotNamed: #ivarForTesting.
	
	ivar propertyAt: #testKeySelector put: 1.
		
	ivar removeProperty: #testKeySelector.
	self
		assert: (ivar propertyAt: #testKeySelector)
		equals: nil.
		
	self assert: ivar properties isNil.