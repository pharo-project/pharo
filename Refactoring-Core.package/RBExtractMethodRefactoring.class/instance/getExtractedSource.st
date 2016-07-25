transforming
getExtractedSource
	| source |
	source := class sourceCodeFor: selector.
	((extractionInterval first between: 1 and: source size) 
		and: [extractionInterval last between: 1 and: source size]) 
			ifFalse: [self refactoringFailure: 'Invalid interval'].
	^source copyFrom: extractionInterval first to: extractionInterval last