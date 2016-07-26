tests
testSmall
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html 
			tag: #h1 with: #Small;
			tag: #p with: 'This is a small HTML document' ].
	self assert: result equals: '<h1>Small</h1><p>This is a small HTML document</p>'