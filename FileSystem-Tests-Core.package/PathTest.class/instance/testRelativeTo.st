tests
testRelativeTo
	"self run: #testRelativeTo"
	"aPath relativeTo: aParent returns a new path relative to the parent"
	
	| parent child relative |
	parent := Path / 'griffle'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative equals: (Path * 'plonk' / 'nurp')