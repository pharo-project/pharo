affecting
push: n
	(position := position + n) > length 
		ifTrue: [length := position]