tests
testBuildingNestedComposites
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list.
					a text.
					a
						composite: [ :another | 
							another text.
							another list ] ].
	browser startOn: 42.
	self assert: ((browser paneNamed: #one) presentations isKindOf: GLMCompositePresentation).
	self assert: (browser paneNamed: #one) presentations size equals: 3.
	self assert: ((browser paneNamed: #one) presentations last isKindOf: GLMCompositePresentation).
	self assert: (browser paneNamed: #one) presentations last size equals: 2