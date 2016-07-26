tests
testSimpleTransmission
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser sendTo: #two -> #port2 from: #one -> #port1.
	self assert: browser transmissions size equals: 1.
	self assert: (browser transmissions first transmissionStrategy isKindOf: GLMNoStrategy).
	self assert: browser transmissions first origins first equals: ((browser paneNamed: #one) port: #port1).
	self assert: browser transmissions first destination equals: ((browser paneNamed: #two) port: #port2)