tests
testAttributeEscaping
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #div attributes: #(query '"literal"') with: 'Foo' ].
	self assert: result equals: '<div query="&quot;literal&quot;">Foo</div>'