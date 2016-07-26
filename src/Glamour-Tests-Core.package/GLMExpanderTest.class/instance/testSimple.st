tests
testSimple
	| expander |
	expander := GLMExpander new.
	expander startOn: #(#a #b #c).
	self assert: expander panes size equals: 3.
	self assert: (expander panes first port: #entity) value equals: #a