tests
testTagWithId
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #div id: #menu with: 'Foo' ].
	self assert: result equals: '<div id="menu">Foo</div>'