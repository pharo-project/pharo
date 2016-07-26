private
growAtFirst
	"Add new empty slots to the front of array, while keeping the empty slots at the end."

	| newArray newFirstIndex newLastIndex |
	newArray := self class arrayType new: (array size * 2 max: 1).
	newFirstIndex := newArray size - array size + firstIndex.
	newLastIndex := newFirstIndex + lastIndex - firstIndex.
	newArray 
		replaceFrom: newFirstIndex
		to: newLastIndex
		with: array
		startingAt: firstIndex.
	array := newArray.
	firstIndex := newFirstIndex.
	lastIndex := newLastIndex