tests
testRemoveAll
	| list2 |
	list add: link1.
	list add: link2.
	list add: link3.
	list add: link4.
	list2 := list copy.
	list removeAll.
	
	self assert: list size = 0.
	self assert: list2 size = 4 description: 'the copy has not been modified'