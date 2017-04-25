tests
testNextInto
	| array |
	array := Array new: 8.
	array := gen next: 8 into: array.
	self assert: (array allSatisfy: [ :each | each isFloat and: [ each >= 0 and: [ each < 1 ] ] ])