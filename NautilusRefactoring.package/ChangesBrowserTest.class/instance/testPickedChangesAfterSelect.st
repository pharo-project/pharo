tests
testPickedChangesAfterSelect
	| item |
	item := RBAddClassChange new.
	self assert: browser pickedChanges isEmpty.
	browser changesTree selectedItem: item.
	self assert: browser pickedChanges size equals: 1.
	self assert: browser pickedChanges first equals: item