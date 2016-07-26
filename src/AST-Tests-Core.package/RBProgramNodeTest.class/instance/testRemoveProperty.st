testing-properties
testRemoveProperty
	self should: [ self node removeProperty: #foo ] raise: Error.
	self node propertyAt: #foo put: true.
	self assert: (self node removeProperty: #foo)