testing
testIncludes
	| list |
	list := DoubleLinkedList new.
	list addAll: (1 to: 10).
	self assert: (list includes: 5).
	self deny: (list includes: 0)