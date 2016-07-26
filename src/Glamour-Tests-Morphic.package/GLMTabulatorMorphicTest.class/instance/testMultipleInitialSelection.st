tests
testMultipleInitialSelection
	| browser firstTreeMorph secondTreeMorph |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | 
					(a tree)
						beMultiple;
						display: [ :x | 1 to: x * 3 ] ].
	window := browser openOn: 5.
	(browser panes first port: #selection) value: #(2 5 3).
	firstTreeMorph := self find: MorphTreeMorph in: window.
	self assert: firstTreeMorph model selection notNil.
	self assert: firstTreeMorph model selection selectedItemOrItemsOrNil size equals: 3.