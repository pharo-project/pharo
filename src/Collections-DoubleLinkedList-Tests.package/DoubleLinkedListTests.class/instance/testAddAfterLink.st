testing
testAddAfterLink
	| list link |
	list := DoubleLinkedList new.
	list add: 1.
	link := list add: 2.
	list add: 3.
	list add: 0 afterLink: link.
	self assert: list asArray equals: #( 1 2 0 3 )