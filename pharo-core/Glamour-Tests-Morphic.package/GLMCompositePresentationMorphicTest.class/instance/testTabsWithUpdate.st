tests
testTabsWithUpdate
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list.
					a text ].
	window := browser openOn: 42.
	self assert: self compositeTabGroup pages size equals: 2.
	self shouldnt: [browser panes first presentations update]  raise: Error