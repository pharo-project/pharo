tests
testStartWithOnlyJumpStart
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #one;
		andShow: [ :a | a list ].
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a text ].
	self assert: browser transmissions size equals: 2.
	self assert: (browser paneNamed: #one) presentations isEmpty.
	self assert: (browser paneNamed: #two) presentations isEmpty.
	browser startOn: #(15 25 35).
	self assert: (browser paneNamed: #one) presentations size equals: 1.
	self assert: (browser paneNamed: #one) matchingPresentations size equals: 1.
	self assert: (browser paneNamed: #two) presentations isEmpty.
	self assert: (browser paneNamed: #two) matchingPresentations isEmpty