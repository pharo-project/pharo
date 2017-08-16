tests
testRelativeFromStringNormalization

	| path |
	
	path := Path from: 'plonk/../griffle'.
	
	self assert: path isRelative.
	self assert: path size equals: 3.
	self assert: path segments equals: #('plonk' '..' 'griffle').