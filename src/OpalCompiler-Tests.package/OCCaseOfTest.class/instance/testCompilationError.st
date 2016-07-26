tests
testCompilationError
	self should: [ self compile: 'methodPattern 1 caseOf: { }' ] raise: SyntaxErrorNotification.
	self should: [ self compile: 'methodPattern 1 caseOf: { } otherwise: [ 42 ]' ] raise: SyntaxErrorNotification.