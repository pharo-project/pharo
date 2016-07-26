tests
testMakeRelative
	"self run: #testMakeRelative"
	
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := parent makeRelative: child.
	self assert: relative equals: (Path parent / 'plonk' / 'nurp')