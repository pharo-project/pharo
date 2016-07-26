testing-properties
testPropertyAtIfAbsentPut
	self assert: (self node propertyAt: #foo ifAbsentPut: [ true ]).
	self assert: (self node propertyAt: #foo ifAbsentPut: [ false ])