accessing
options
	^options isNil ifTrue: [self class refactoringOptions] ifFalse: [options]