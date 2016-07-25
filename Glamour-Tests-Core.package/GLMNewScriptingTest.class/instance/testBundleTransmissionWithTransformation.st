tests
testBundleTransmissionWithTransformation
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	(browser transmit)
		to: #one;
		andShow: [ :a | a text ].
	(browser transmit)
		from: #one transformed: [ :x | x + 1 ];
		to: #two;
		andShow: [ :a | a text ].
	browser startOn: 1.
	((browser paneNamed: #one) port: #selection) value: 41.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 42