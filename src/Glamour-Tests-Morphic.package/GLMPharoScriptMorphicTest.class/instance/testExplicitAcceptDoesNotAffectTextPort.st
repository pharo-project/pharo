tests
testExplicitAcceptDoesNotAffectTextPort
	| composite textMorph |
	composite := GLMCompositePresentation new with: [ :a | a pharoScript display: '123' ].
	window := composite openOn: 4.

	window position: 0@0.
	window extent > Display extent ifTrue: [ 
		"ignore test if the display size is too small and we can not simulate events properly"
		^ self ].
	
	textMorph := self find: RubScrolledTextMorph in: window.
	textMorph simulateClick.
	self simulateKeyStrokes: '4'.
	self assert: (composite pane port: #text) value = '1234'.
	self simulateKeyStroke: $s meta.
	self simulateKeyStrokes: '56'.
	self assert: (composite pane port: #text) value = '123456'