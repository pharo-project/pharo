private
fixCollisionsFrom: start
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one"

	| element index |
	index := start.
	[ (element := array at: (index := index \\ array size + 1)) isNil ] whileFalse: [
		| newIndex |
		(newIndex := self findElementOrNil: (keyBlock value: element)) = index ifFalse: [
			array swap: index with: newIndex ] ]