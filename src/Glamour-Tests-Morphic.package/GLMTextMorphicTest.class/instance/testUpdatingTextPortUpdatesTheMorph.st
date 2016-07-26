tests
testUpdatingTextPortUpdatesTheMorph
	| composite textMorph |
	composite := GLMCompositePresentation new with: [:a | a text ].
	window := composite openOn: 'fourtytwo'.
	composite first text: '42'.
	self assert: (composite pane port: #text) value asString equals: '42'.
	textMorph := self find: GLMPluggableTextMorph in: window.
	self assert: textMorph text asString equals: '42'