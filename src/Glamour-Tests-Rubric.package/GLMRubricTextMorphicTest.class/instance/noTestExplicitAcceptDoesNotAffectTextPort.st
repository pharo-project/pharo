tests
noTestExplicitAcceptDoesNotAffectTextPort
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123' ].
	window := composite openOn: 4.
	textMorph := self find: RubScrolledTextMorph in: window.
	self simulateOnlyOneClickOn: textMorph.
	self simulateKeyStrokes: '4'.
	self assert: (composite pane port: #text) value asString equals: '1234'.
	self simulateKeyStroke: $s meta.
	self simulateKeyStrokes: '56'.
	self assert: (composite pane port: #text) value asString equals: '123456'