tests
testDefault
	| browser |
	browser := GLMTabulator new.
	browser startOn: #(1 2 3).
	self assert: (browser panes first presentations first isKindOf: GLMFlexiblePresentation).
	browser startOn: 1.
	self assert: (browser panes first presentations first isKindOf: GLMFlexiblePresentation).