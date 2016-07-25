tests
testStartTransmission
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #one;
		andShow: [ :a | a list ].
	self assert: browser transmissions size equals: 1.
	self assert: (browser paneNamed: #one) presentations isEmpty.
	self assert: (browser paneNamed: #two) presentations isEmpty.
	browser startOn: 15.
	self assert: (browser paneNamed: #one) presentations size equals: 1.
	self assert: (browser paneNamed: #two) presentations isEmpty