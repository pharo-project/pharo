tests
testRelativeToBranch
	| parent child relative |
	parent := Path / 'griffle' / 'bibb'.
	child := Path / 'griffle' / 'plonk' / 'nurp'.
	relative := child relativeTo: parent.
	self assert: relative  equals: (Path parent / 'plonk' / 'nurp')