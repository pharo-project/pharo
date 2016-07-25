tests
testEscaping
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #div with: 'Foo & Bar < 100' ].
	self assert: result equals: '<div>Foo &amp; Bar &lt; 100</div>'.
	result := ZnHtmlOutputStream streamContents: [ :html |
		html << 'Foo & Bar < 100' ].
	self assert: result equals: 'Foo & Bar < 100'.
	result := ZnHtmlOutputStream streamContents: [ :html |
		html str: 'Foo & Bar < 100' ].
	self assert: result equals: 'Foo &amp; Bar &lt; 100'.