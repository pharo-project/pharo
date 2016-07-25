visitor-double dispatching
visitMessageNode: aMessageNode
	aMessageNode receiver: (self visitNode: aMessageNode receiver).
	aMessageNode arguments: (aMessageNode arguments collect: [ :each | self visitNode: each ])