accessing
workingCopy
	| pkg |
	workingCopy ~~ nil ifTrue: [ ^workingCopy ].
	pkg := MCPackage named: self configurationClass category asString.
	pkg  hasWorkingCopy ifFalse: [ ^nil ].
	^workingCopy := pkg workingCopy