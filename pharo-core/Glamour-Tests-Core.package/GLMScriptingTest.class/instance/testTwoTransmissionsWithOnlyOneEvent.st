tests
testTwoTransmissionsWithOnlyOneEvent
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a list ].
	browser
		transmit to: #three;
		from: #two;
		andShow: [ :a | a text ].
	self assert: browser transmissions size equals: 2.
	self assert: (browser paneNamed: #one) presentations isEmpty.
	self assert: (browser paneNamed: #two) presentations isEmpty.
	self assert: (browser paneNamed: #three) presentations isEmpty.
	((browser paneNamed: #one) port: #selection) value: 42.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 42.
	self assert: (browser paneNamed: #one) presentations isEmpty.
	self assert: (browser paneNamed: #two) presentations size equals: 1.
	self assert: (browser paneNamed: #two) matchingPresentations size equals: 1.
	self assert: (browser paneNamed: #three) presentations isEmpty.
	self assert: (browser paneNamed: #three) matchingPresentations isEmpty