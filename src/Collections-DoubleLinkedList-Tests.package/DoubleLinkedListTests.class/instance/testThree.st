testing
testThree
	| list |
	list := DoubleLinkedList new.
	1 to: 3 do: [ :each | list add: each ].
	self deny: list isEmpty.
	self assert: list first equals: 1.
	self assert: list last equals: 3.
	self assert: list asArray equals: #( 1 2 3 )