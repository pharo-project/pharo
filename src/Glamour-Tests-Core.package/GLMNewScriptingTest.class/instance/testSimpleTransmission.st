tests
testSimpleTransmission
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	(browser transmit)
		from: #one port: #port1;
		fromOutsidePort: #outerPort;
		to: #two port: #port2.
	(browser transmit)
		from: #one port: #port3;
		toOutsidePort: #anotherOuterPort.
	self assert: browser transmissions size equals: 2.
	self assert: browser transmissions first browser == browser.
	self assert: (browser transmissions first transmissionStrategy isKindOf: GLMNoStrategy).
	self assert: browser transmissions first origins first equals: ((browser paneNamed: #one) port: #port1).
	self assert: browser transmissions first origins last equals: (browser pane port: #outerPort).
	self assert: browser transmissions first destination equals: ((browser paneNamed: #two) port: #port2).
	self assert: browser transmissions last browser == browser.
	self assert: (browser transmissions last transmissionStrategy isKindOf: GLMNoStrategy).
	self assert: browser transmissions last origins first equals: ((browser paneNamed: #one) port: #port3).
	self assert: browser transmissions last destination equals: (browser pane port: #anotherOuterPort)