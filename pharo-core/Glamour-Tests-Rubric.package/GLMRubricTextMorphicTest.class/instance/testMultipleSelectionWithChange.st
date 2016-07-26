tests
testMultipleSelectionWithChange
	| composite textMorph textPresentation |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123456789' ].
	window := composite openOn: 4.
	textMorph := self find: RubScrolledTextMorph in: window.
	self assert: textMorph selectionInterval isEmpty.
	textPresentation := composite presentations first.
	textPresentation selectionInterval: (2 to: 5).
	self assert: textMorph textArea selection asString equals: '2345'.
	self assert: textPresentation selectedText asString equals: '2345'