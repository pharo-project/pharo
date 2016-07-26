tests
testDefaultPresentation
	| dynamic |
	dynamic := GLMDynamicPresentation new.
	dynamic entity: 42.
	dynamic transformation: [ :entity | GLMRubricTextPresentation new ].
	self assert: dynamic currentPresentation presentations size equals: 1.
	self assert: (dynamic currentPresentation presentations first isKindOf: GLMRubricTextPresentation).
	self assert: dynamic currentPresentation pane == dynamic pane