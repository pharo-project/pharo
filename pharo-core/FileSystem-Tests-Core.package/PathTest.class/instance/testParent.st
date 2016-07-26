tests
testParent
	| path parent |
	path := (Path * 'plonk') / 'griffle'.
	parent := path parent.
	self assert: parent isRelative.
	self assert: (parent at: 1) equals: 'plonk'