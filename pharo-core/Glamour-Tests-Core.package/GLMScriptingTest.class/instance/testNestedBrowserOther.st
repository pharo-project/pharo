tests
testNestedBrowserOther
	| browser innerBrowser realInnerBrowser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	innerBrowser := GLMTabulator new.
	innerBrowser column: #innerOne.
	innerBrowser
		transmit to: #innerOne;
		andShow: [ :a | a text ].
	innerBrowser transmit toOutsidePort: #selection; from: #innerOne.
	browser
		transmit to: #one;
		andShow: [ :a | (a custom: innerBrowser) display: [ :x | x + 2 ] ].
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a text ].
	browser startOn: 40.
	realInnerBrowser := (browser paneNamed: #one) presentations first.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 40.
	self assert: (realInnerBrowser pane port: #entity) value equals: 40.
	self assert: ((realInnerBrowser paneNamed: #innerOne) port: #entity) value equals: 42.
	(realInnerBrowser panes first port: #selection) value: 7.
	self assert: (realInnerBrowser pane port: #selection) value equals: 7.
	self assert: (browser panes last port: #entity) value equals: 7