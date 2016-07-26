testing-properties
testRemovePropertyIfAbsent
	self assert: (self node removeProperty: #foo ifAbsent: [ true ]).
	self node propertyAt: #foo put: true.
	self assert: (self node removeProperty: #foo ifAbsent: [ false ])