tests
testPastingUpdatesTextPort
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a text display: '' ].
	window := composite openOn: 42.
	textMorph := self find: RubScrolledTextMorph in: window.
	Clipboard clipboardText: '42'.
	textMorph textArea editor paste.
	self assert: (composite pane port: #text) value = '42'