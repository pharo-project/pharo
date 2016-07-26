tests
noTestEnteringTextInPort
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a text display: '' ].
	window := composite openOn: 4.
	textMorph := self find: RubScrolledTextMorph in: window.
	self simulateOnlyOneClickOn: textMorph.
	self simulateKeyStrokes: 'hello'.
	self assert: (composite pane port: #text) value asString equals: 'hello'