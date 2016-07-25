testing
testOne
	| list |
	list := DoubleLinkedList new.
	list add: #one.
	self deny: list isEmpty.
	self assert: list first equals: #one.
	self assert: list last equals: #one.
	self assert: list asArray equals: #( one )