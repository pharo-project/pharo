tests
testTagWithEmptyAttributes
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #div attributes: #(class menu id one readonly nil) with: 'Foo' ].
	self assert: result equals: '<div class="menu" id="one" readonly>Foo</div>'