examples - branches
exampleIfTrueWithNilAssertion
	| tmp1 |
	tmp1 := 1.
	tmp1 > 2
		ifTrue: [ tmp1 := nil ]