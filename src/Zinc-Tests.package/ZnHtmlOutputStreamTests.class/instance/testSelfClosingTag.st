tests
testSelfClosingTag
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #br ].
	self assert: result equals: '<br/>'