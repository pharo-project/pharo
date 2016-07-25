test - properties
testRemoveProperty
	self class propertyAt: #testKeySelector put: 1.
	self class removeProperty: #testKeySelector.
	self assert: (self class propertyAt: #testKeySelector) equals: nil