accessing
atLast: indexFromEnd ifAbsent: block
	"Return element at indexFromEnd from the last position.
	 atLast: 1 ifAbsent: [] returns the last element"

	^ self at: self size + 1 - indexFromEnd ifAbsent: block