testing-properties
testPropertyAt
	self should: [ self node propertyAt: #foo ] raise: Error.
	self node propertyAt: #foo put: true.
	self assert: (self node propertyAt: #foo)