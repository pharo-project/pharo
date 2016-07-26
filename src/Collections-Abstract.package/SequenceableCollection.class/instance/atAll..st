accessing
atAll: indexArray
	"Answer a new collection like the receiver which contains all elements
	of the receiver at the indices of indexArray."
	"#('one' 'two' 'three' 'four') atAll: #(3 2 4)"

	| newCollection |
	newCollection := self species ofSize: indexArray size.
	1 to: indexArray size do:
		[:index |
		newCollection at: index put: (self at: (indexArray at: index))].
	^ newCollection