tests
testNestedCompositesPaneReference
	| browser | 
	browser := GLMTabulator new. 
	browser column: #one. 
	browser transmit to: #one; andShow: [ :a |
		a list.
		a text.
		a composite: [:another | 
			another text.
			another list ] ].
	browser startOn: 42.
	(browser paneNamed: #one) presentations do: [:each |
		self assert: each pane == (browser paneNamed: #one) ].
	(browser paneNamed: #one) presentations last do: [:each |
		self assert: each pane == (browser paneNamed: #one) ]