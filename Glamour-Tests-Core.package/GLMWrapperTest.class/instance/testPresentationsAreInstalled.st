tests
testPresentationsAreInstalled
	| browser |
	browser := GLMWrapper new.
	browser show: [ :a | a text ].
	browser startOn: 42.
	self assert: browser panes anyOne presentations size equals: 1.
	self assert: (browser panes anyOne presentations first isKindOf: GLMRubricTextPresentation)