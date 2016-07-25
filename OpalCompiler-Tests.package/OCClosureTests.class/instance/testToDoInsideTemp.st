testing-todo
testToDoInsideTemp
	1 to: 5 do: [ :index | 
		| temp | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(1 2 3 4 5)