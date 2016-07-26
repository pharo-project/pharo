tests
testEncoding
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #div with: 'élève en Français' ].
	self assert: result equals: '<div>élève en Français</div>'