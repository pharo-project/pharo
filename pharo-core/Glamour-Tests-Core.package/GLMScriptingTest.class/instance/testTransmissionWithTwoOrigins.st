tests
testTransmissionWithTwoOrigins
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	browser
		transmit to: #three;
		from: #one;
		from: #two;
		andShow: [ :a | a text ].
	((browser paneNamed: #one) port: #selection) value: 42.
	self assert: ((browser paneNamed: #three) port: #entity) value first equals: 42.
	self assert: ((browser paneNamed: #three) port: #entity) value last equals: nil.
	self assert: (browser paneNamed: #three) presentations size equals: 1.
	self assert: (browser paneNamed: #three) matchingPresentations size equals: 0.
	((browser paneNamed: #two) port: #selection) value: 43.
	self assert: ((browser paneNamed: #three) port: #entity) value first equals: 42.
	self assert: ((browser paneNamed: #three) port: #entity) value last equals: 43.
	self assert: (browser paneNamed: #three) matchingPresentations size equals: 1