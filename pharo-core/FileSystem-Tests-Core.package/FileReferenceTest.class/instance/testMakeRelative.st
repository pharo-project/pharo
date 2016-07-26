tests
testMakeRelative
	"self run: #testMakeRelative"

	| parent child relative |
	parent := filesystem / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := parent makeRelative: child.
	self assert: relative = (Path * 'plonk' / 'nurb')