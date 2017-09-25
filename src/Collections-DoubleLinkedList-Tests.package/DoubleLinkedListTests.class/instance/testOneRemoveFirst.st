testing
testOneRemoveFirst
	| list |
	list := DoubleLinkedList new.
	list add: #one.
	list removeFirst.
	self assert: list isEmpty.
	list add: #one.
	list removeLast.
	self assert: list isEmpty