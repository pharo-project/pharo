tests
testTextPortPopulatedByDefault
	| composite |
	composite := GLMCompositePresentation new with: [:a | a text ].
	window := composite openOn: '42'.
	self assert: (composite pane port: #text) value asString equals: '42'