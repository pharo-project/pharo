testing
testThreeRemoveLast
	| list link |
	list := DoubleLinkedList new.
	1 to: 3 do: [ :each | list add: each ].
	link := list removeLast.
	self assert: link value equals: 3.
	self assert: list first equals: 1.
	self assert: list last equals: 2.
	self assert: list asArray equals: #( 1 2 )