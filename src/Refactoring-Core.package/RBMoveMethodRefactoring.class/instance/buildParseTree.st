private
buildParseTree
	parseTree := (class parseTreeFor: selector) copy.
	parseTree isNil ifTrue: [self refactoringFailure: 'Could not parse method']