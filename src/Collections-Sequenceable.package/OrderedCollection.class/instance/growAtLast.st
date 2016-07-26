private
growAtLast
	"Add new empty slots to the end of array, while keeping the empty slots at the front."

	| newArray |
	newArray := self class arrayType new: (array size * 2 max: 1).
	newArray 
		replaceFrom: firstIndex
		to: lastIndex
		with: array
		startingAt: firstIndex.
	array := newArray