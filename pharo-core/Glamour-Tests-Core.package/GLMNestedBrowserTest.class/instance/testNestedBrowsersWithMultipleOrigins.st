tests
testNestedBrowsersWithMultipleOrigins
	| browser innerBrowser transmittedOne transmittedTwo |
	innerBrowser := GLMTabulator new.
	innerBrowser row: #inner.
	(innerBrowser transmit)
		to: #inner;
		andShow: [ :a | a text ].
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	(browser transmit)
		to: #three;
		from: #one;
		from: #two;
		andShow: [ :a | 
					a
						custom:
							(innerBrowser
								display: [ :one :two | 
									transmittedOne := one.
									transmittedTwo := two ]) ].
	browser startOn: 1.
	((browser paneNamed: #one) port: #selection) value: 1.
	((browser paneNamed: #two) port: #selection) value: 2.
	self assert: transmittedOne equals: 1.
	self assert: transmittedTwo equals: 2