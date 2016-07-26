testing-todo
testToDoInsideTempNotInlined
	| block |
	block := [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	1 to: 5 do: block.
	self assertValues: #(1 2 3 4 5)