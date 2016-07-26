tests
testHtml5Tag
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html html5 ].
	self assert: result equals: '<!DOCTYPE html>'