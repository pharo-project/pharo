tests
testTagWithMultipleAttributes
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #div attributes: #(class menu id one) with: 'Foo' ].
	self assert: result equals: '<div class="menu" id="one">Foo</div>'