accessing
workingCopy
	| pkg |
	workingCopy ~~ nil ifTrue: [ ^workingCopy ].
	pkg := MCPackage named: self packageName.
	pkg  hasWorkingCopy ifFalse: [ ^nil ].
	^workingCopy := pkg workingCopy