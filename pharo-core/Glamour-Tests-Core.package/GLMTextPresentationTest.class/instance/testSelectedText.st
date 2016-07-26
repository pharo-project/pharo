tests
testSelectedText
	| presentation |
	presentation := GLMRubricTextPresentation new.
	self assert: presentation selectedText equals: ''.
	presentation selectedTextBlock: [ 'selection' ].
	self assert: presentation selectedText equals: 'selection'