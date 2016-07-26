testing
toDoLimit: variable
	(receiver = variable and: [selector key = #<= or: [selector key = #>=]]) 
		ifTrue: [^ arguments first]
		ifFalse: [^ nil]