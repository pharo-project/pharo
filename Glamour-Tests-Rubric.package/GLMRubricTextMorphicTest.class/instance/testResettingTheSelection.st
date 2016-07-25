tests
testResettingTheSelection
	| composite textPresentation |
	composite := GLMCompositePresentation new with: [ :a | a text display: '123456789' ].
	window := composite openOn: 4.
	textPresentation := composite presentations first.
	textPresentation selectionInterval: (2 to: 5).
	self shouldnt: [textPresentation selectionInterval: nil] raise: Error