private-accessing
selectedSource
	| source |
	source := class sourceCodeFor: selector.
	source isNil ifTrue: [self refactoringFailure: 'Couldn''t find sources'].
	((sourceInterval first between: 1 and: source size) 
		and: [sourceInterval last between: 1 and: source size]) 
			ifFalse: [self refactoringFailure: 'Invalid interval'].
	^source copyFrom: sourceInterval first to: sourceInterval last