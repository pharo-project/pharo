next
testNextWithAnOpeningCommentAtEndGetError
	| source |
	
	source := 'self "'.
	self should: [(self buildScannerForText: source) next] raise: SyntaxErrorNotification