tests
testResolveRelative
	| path |
	path := Path * 'griffle'.
	self assert: path resolve == path.
	self assert: path isRelative