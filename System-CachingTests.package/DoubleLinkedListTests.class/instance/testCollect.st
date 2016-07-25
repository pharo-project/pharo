testing
testCollect
	| list doubles |
	list := DoubleLinkedList new.
	list addAll: #(1 2 3).
	doubles := list collect: [ :each | each * 2 ].
	self assert: doubles asArray equals: #(2 4 6)