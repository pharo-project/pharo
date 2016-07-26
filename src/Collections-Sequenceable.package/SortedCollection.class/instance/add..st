adding
add: newObject
	^ super insert: newObject before: (self indexForInserting: newObject)