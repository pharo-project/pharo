public access
undoOperation
	undo isEmpty
		ifTrue: [ ^ self ].
	self ignoreChangesWhile: [ 
		| change |
		change := undo removeLast.
		redo add: change execute ]