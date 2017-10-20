testing
testAddBeforeLink
	| list link |
	list := DoubleLinkedList new.
	list add: 1.
	link := list add: 2.
	list add: 3.
	list add: 0 beforeLink: link.
	self assert: list asArray equals: #( 1 0 2 3 )