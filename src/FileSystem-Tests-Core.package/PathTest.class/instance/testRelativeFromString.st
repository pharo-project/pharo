tests
testRelativeFromString

	| path |
	
	path := Path from: 'plonk/griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 2.
	self assert: (path at: 1) equals: 'plonk'.
	self assert: (path at: 2) equals: 'griffle'.