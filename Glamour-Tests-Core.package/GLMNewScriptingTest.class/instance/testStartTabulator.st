tests
testStartTabulator
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					a list.
					a text ].
	browser startOn: 42.
	self assert: ((browser paneNamed: #one) presentations isKindOf: GLMCompositePresentation).
	self assert: (browser paneNamed: #one) presentations size equals: 2