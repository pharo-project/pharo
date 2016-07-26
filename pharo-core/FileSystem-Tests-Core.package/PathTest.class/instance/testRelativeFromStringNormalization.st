tests
testRelativeFromStringNormalization

	| path |
	
	path := Path from: 'plonk/../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 1.
	self assert: (path at: 1) equals: 'griffle'.