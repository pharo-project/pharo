testing-todo
testToDoArgumentNotInlined
	| block |
	block := [ :index |
		collection add: [ index ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)