tests
testPresentations
	| expander |
	expander := GLMExpander new.
	expander show: [ :a | a text ].
	expander startOn: #(#a #b #c).
	self assert: expander panes size equals: 3.
	self assert: expander panes first presentations entity value equals: #a.
	self assert: expander panes first presentations size equals: 1.
	self assert: (expander panes first presentations first isKindOf: GLMRubricTextPresentation)