accessing
atLast: indexFromEnd
	"Return element at indexFromEnd from the last position.
	 atLast: 1, returns the last element"

	^ self atLast: indexFromEnd ifAbsent: [self error: 'index out of range']