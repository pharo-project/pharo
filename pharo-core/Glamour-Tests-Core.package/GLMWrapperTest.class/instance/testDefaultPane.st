tests
testDefaultPane
	| browser |
	browser := GLMWrapper new.
	self assert: browser panes size equals: 1