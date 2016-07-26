testing-properties
testHasProperty
	self deny: (self node hasProperty: #foo).
	self node propertyAt: #foo put: 123.
	self assert: (self node hasProperty: #foo)