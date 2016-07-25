public access
redoOperation
	redo isEmpty
		ifTrue: [ ^ self ].
	self ignoreChangesWhile: [ 
		| change |
		change := redo removeLast.
		undo add: change execute ]