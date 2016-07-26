next
testNextWithAnOpeningCommentGetError
	| source |
	
	source := '"only the opening'.
	self should: [(self buildScannerForText: source) next] raise: SyntaxErrorNotification