testing
testEmpty
	| list |
	list := DoubleLinkedList new.
	self assert: list isEmpty.
	self should: [ list first ] raise: CollectionIsEmpty.
	self should: [ list last ] raise: CollectionIsEmpty.
	list do: [ :each | self fail ]
	