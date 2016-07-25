testing
testThreeRemoveFirst
	| list link |
	list := DoubleLinkedList new.
	1 to: 3 do: [ :each | list add: each ].
	link := list removeFirst.
	self assert: link value equals: 1.
	self assert: list first equals: 2.
	self assert: list last equals: 3.
	self assert: list asArray equals: #( 2 3 )