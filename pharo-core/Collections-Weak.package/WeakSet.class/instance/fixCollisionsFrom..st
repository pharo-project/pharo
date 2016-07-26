private
fixCollisionsFrom: start
	"The element at start has been removed and replaced by flag.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| element index |
	index := start.
	[ (element := array at: (index := index \\ array size + 1)) == flag ] whileFalse: [
		element 
			ifNil: [ "This object is gone"
				array at: index put: flag.
				tally := tally - 1 ]
			ifNotNil: [
				| newIndex |
				(newIndex := self scanFor: element enclosedSetElement) = index ifFalse: [
					array 
						at: newIndex put: element;
						at: index put: flag ] ] ]
