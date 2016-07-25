tests
testRemoveCategory
	self assert: self organization categories size = 2.
	self should: [ self organization removeCategory: 'one' ] raise: Error.
	self organization removeCategory: 'empty'.
	self assert: self organization categories size = 1.
	self assert: self organization categories first = 'one'