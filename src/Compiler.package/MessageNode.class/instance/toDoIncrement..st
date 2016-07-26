testing
toDoIncrement: variable
	(receiver = variable and: [selector key = #+]) 
		ifFalse: [^ nil].
	arguments first isConstantNumber
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]