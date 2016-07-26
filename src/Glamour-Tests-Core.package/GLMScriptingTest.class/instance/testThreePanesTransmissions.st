tests
testThreePanesTransmissions
	| browser two |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	(browser transmit)
		to: #three;
		from: #one;
		passivelyFrom: #two;
		andShow: [ :a | a text display: [ :x :y | two := y ] ].
	self assert: browser transmissions size equals: 1.
	self assert: browser transmissions first passiveOrigins size equals: 1.
	self assert: browser transmissions first activeOrigins size equals: 1.
	((browser paneNamed: #two) port: #selection) value: 15.
	self assert: ((browser paneNamed: #two) port: #selection) value equals: 15.
	self assert: ((browser paneNamed: #three) port: #entity) value isNil.
	((browser paneNamed: #one) port: #selection) value: 20.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 20.
	self assert: ((browser paneNamed: #three) port: #entity) value first equals: 20.
	self assert: ((browser paneNamed: #three) port: #entity) value last equals: 15