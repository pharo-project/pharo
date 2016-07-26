tests
testBundleTransmission
	| browser transmission |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	(browser transmit)
		from: #one port: #port1;
		from: #two port: #port2;
		to: #three -> #port3;
		andShow: [ :a | 
					a list.
					a table ].
	transmission := browser transmissions first.
	self assert: browser transmissions size equals: 1.
	self assert: transmission browser == browser.
	self assert: (transmission transmissionStrategy isKindOf: GLMReplacePresentationsStrategy).
	self assert: transmission origins size equals: 2.
	self assert: transmission origins first equals: ((browser paneNamed: #one) port: #port1).
	self assert: transmission origins last equals: ((browser paneNamed: #two) port: #port2).
	self assert: transmission destination equals: ((browser paneNamed: #three) port: #port3).
	self assert: (transmission transmissionStrategy presentations isKindOf: GLMCompositePresentation).
	self assert: transmission transmissionStrategy presentations size equals: 2