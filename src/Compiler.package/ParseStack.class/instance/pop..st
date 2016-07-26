accessing
pop: n

	(position := position - n) < 0 
		ifTrue: [self error: 'Parse stack underflow']