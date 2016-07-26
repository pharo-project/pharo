tests
testComment
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html comment: 'a comment' ].
	self assert: result equals: '<!-- a comment -->'