tests
testFormat
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html format: '{1} & {2}' with: { '<'. '>' } ].
	self assert: result equals: '&lt; &amp; >'