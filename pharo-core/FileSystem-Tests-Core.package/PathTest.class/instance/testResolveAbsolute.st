tests
testResolveAbsolute
	| path |
	path := Path / 'griffle'.
	self assert: path resolve == path.
	self assert: path isAbsolute