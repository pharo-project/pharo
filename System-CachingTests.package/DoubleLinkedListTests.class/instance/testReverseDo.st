testing
testReverseDo
	| list value |
	list := DoubleLinkedList new.
	list addAll: (1 to: 10).
	value := 10.
	list reverseDo: [ :each |
		self assert: each equals: value.
		value := value - 1 ]