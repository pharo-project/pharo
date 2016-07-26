tests - adding uniquely
testAddAlreadyThereDoesNotCount

	| added oldSize |
	oldSize := self collectionWithElement size.
	self assert: (self collectionWithElement includes: self element).

	added := self collectionWithElement add: self element.
	
	self assert: added = self element.
	self assert: (self collectionWithElement includes: self element).
	self assert: self collectionWithElement size = oldSize.