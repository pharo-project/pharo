tests
testTagWithClass
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #div class: #menu with: 'Foo' ].
	self assert: result equals: '<div class="menu">Foo</div>'