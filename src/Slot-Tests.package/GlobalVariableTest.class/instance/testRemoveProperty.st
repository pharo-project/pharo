tests  - properties
testRemoveProperty

	self class binding propertyAt: #testKeySelector put: 1.
		
	self class binding removeProperty: #testKeySelector.
	self
		assert: (self class binding propertyAt: #testKeySelector)
		equals: nil.
		
	self assert: self class binding properties isNil.