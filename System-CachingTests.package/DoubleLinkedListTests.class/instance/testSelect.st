testing
testSelect
	| list even |
	list := DoubleLinkedList new.
	list addAll: (1 to: 10).
	even := list select: [ :each | each even ].
	self assert: even asArray equals: #(2 4 6 8 10)