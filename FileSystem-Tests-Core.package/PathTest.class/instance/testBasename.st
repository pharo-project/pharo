tests
testBasename
	| path |
	path := Path * 'plonk' / 'griffle'.
	self assert: path basename equals: 'griffle'