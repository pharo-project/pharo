testing-todo
testToDoOutsideTemp
	| temp |
	1 to: 5 do: [ :index | 
		temp := index. 
		collection add: [ temp ] ].
	self assertValues: #(5 5 5 5 5)