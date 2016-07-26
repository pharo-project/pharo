tests
testPastingUpdatesTextPort
	| composite textMorph  |
	composite := GLMCompositePresentation new with: [ :a | a text display: '' ].
	window := composite openOn: 42.
	textMorph := self find: RubEditingArea in: window.
	Clipboard clipboardText: '42'.
	textMorph editor paste.
	self assert: (composite pane port: #text) value asString equals: '42'