affecting
pop: n

	(position := position - n) < 0 
		ifTrue: [self error: 'stack underflow in bytecode generator']