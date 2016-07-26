tests
testAllRecursivePresentations
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a text ].
	browser startOn: 42.
	self assert: browser panes first withAllPresentations size equals: 2.
	self assert: browser pane withAllPresentations size equals: 4