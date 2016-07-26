tests
testSimpleTransmissionWithTransformation
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	(browser transmit)
		from: #one port: #port1;
		fromOutsidePort: #entity;
		to: #two port: #port2;
		when: [ :one :outer | one notNil and: [ outer notNil ] ];
		transformed: [ :one :outer | one + outer ].
	browser startOn: 2.
	self assert: ((browser paneNamed: #two) port: #port2) value isNil.
	((browser paneNamed: #one) port: #port1) value: 40.
	self assert: ((browser paneNamed: #two) port: #port2) value equals: 42