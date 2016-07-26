tests
testRelativeToPath
	| parentPath child relative |
	parentPath := Path / 'griffle'.
	child := filesystem / 'griffle' / 'plonk' / 'nurb'.
	relative := child relativeTo: parentPath.
	self assert: relative = (Path * 'plonk' / 'nurb')