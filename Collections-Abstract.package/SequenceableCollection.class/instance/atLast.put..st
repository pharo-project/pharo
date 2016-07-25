accessing
atLast: indexFromEnd put: obj
	"Set the element at indexFromEnd from the last position.
	 atLast: 1 put: obj, sets the last element"

	^ self at: self size + 1 - indexFromEnd put: obj