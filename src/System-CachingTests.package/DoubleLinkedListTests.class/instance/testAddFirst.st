testing
testAddFirst
	| list |
	list := DoubleLinkedList new.
	1 to: 3 do: [ :each | list add: each ].
	list addFirst: 0.
	self deny: list isEmpty.
	self assert: list first equals: 0.
	self assert: list last equals: 3.
	self assert: list asArray equals: #( 0 1 2 3 )