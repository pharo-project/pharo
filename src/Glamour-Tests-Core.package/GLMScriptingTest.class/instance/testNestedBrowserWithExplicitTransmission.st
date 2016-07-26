tests
testNestedBrowserWithExplicitTransmission
	| browser innerBrowser realInnerBrowser |
	browser := GLMTabulator new.
	browser column: #one.
	innerBrowser := GLMTabulator new.
	innerBrowser column: #innerOne.
	(innerBrowser transmit)
		to: #innerOne;
		from: #outer port: #entity;
		andShow: [ :a | a text ].
	(browser transmit)
		to: #one;
		andShow: [ :a | a custom: innerBrowser ].
	browser startOn: 42.
	realInnerBrowser := (browser paneNamed: #one) presentations first.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 42.
	self assert: ((realInnerBrowser paneNamed: #innerOne) port: #entity) value equals: 42