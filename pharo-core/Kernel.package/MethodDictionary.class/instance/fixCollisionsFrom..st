private
fixCollisionsFrom: start
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| key index |
	index := start.
	[ (key := self basicAt: (index := index \\ array size + 1)) == nil ] whileFalse: [
		| newIndex |
		(newIndex := self findElementOrNil: key) = index ifFalse: [
			self swap: index with: newIndex ] ]