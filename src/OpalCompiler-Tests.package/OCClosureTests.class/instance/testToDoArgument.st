testing-todo
testToDoArgument
	1 to: 5 do: [ :index |
		collection add: [ index ] ].
	self assertValues: #(1 2 3 4 5)