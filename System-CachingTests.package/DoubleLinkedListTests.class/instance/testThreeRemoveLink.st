testing
testThreeRemoveLink
	| list link |
	list := DoubleLinkedList new.
	list add: 1.
	link := list add: 2.
	list add: 3.
	list removeLink: link.
	self assert: list first equals: 1.
	self assert: list last equals: 3.
	self assert: list asArray equals: #( 1 3 )