testing
testOneRemoveLast
	| list |
	list := DoubleLinkedList new.
	list add: #one.
	list removeLast.
	self assert: list isEmpty.
	list add: #one.
	list removeLast.
	self assert: list isEmpty