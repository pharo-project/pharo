tests
testStartTransmissionFromInitializationBlock
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #one;
		andShow: [ :a | 
			a list
				initialize: [ :pres | pres selection: 4 ] ].
	browser 
		transmit 
		from:#one port: #selection;
		to:#two port: #entity;
		andShow: [ :a |
			a text ].
	self assert: (browser paneNamed: #one) presentations isEmpty.
	self assert: (browser paneNamed: #two) presentations isEmpty.
	browser startOn: 42.
	self assert: (browser paneNamed: #one) presentations size equals: 1.
	self assert: (browser paneNamed: #one) presentations first selection equals: 4.
	self assert: (browser paneNamed: #two) presentations size equals: 1