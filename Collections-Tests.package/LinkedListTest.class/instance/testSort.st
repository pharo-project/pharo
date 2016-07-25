requirements
testSort
	|aLinkedList|
	 aLinkedList := (LinkedList with: 1 with: 2).
	aLinkedList  sort: [ :a :b | a > b].
	self assert: (aLinkedList = (LinkedList with: 2 with: 1)).