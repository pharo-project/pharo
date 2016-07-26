tests - adding uniquely
testAddNewElementIncrementsSize
	| added oldSize |
	oldSize := self collectionWithoutElement size.
	self deny: (self collectionWithoutElement includes: self element).
	added := self collectionWithoutElement add: self element.
	self assert: added = self element.
	self assert: (self collectionWithoutElement includes: self element).
	self assert: self collectionWithoutElement size = (oldSize + 1)