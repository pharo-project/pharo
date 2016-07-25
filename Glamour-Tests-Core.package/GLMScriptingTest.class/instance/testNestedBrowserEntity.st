tests
testNestedBrowserEntity
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
		andShow: [ :a | (a custom: innerBrowser) display: [ :x | x + 2 ] ].
	browser startOn: 40.
	realInnerBrowser := (browser paneNamed: #one) presentations first.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 40.
	self assert: (realInnerBrowser pane port: #entity) value equals: 40.
	self assert: ((realInnerBrowser paneNamed: #innerOne) port: #entity) value equals: 42