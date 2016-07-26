tests
testSelectionValidation
	| browser |
	browser := GLMTabulator new.
	browser
		column: [ :c | 
					c
						row: #one;
						row: #two ];
		column: #three.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	(browser transmit)
		to: #two;
		from: #one;
		andShow: [ :a | 
					(a list)
						beMultiple;
						display: [ :x | 1 to: x ] ].
	(browser transmit)
		to: #three;
		from: #two;
		andShow: [ :a | a text ].
	browser startOn: 42.
	((browser paneNamed: #one) port: #selection) value: 40.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 40.
	((browser paneNamed: #two) port: #selection) value: #(10 30 40).
	self assert: ((browser paneNamed: #two) port: #selection) value equals: #(10 30 40).
	self assert: ((browser paneNamed: #three) port: #entity) value equals: #(10 30 40).
	((browser paneNamed: #two) port: #selection) value: #(10 30).
	self assert: ((browser paneNamed: #two) port: #selection) value equals: #(10 30).
	self assert: ((browser paneNamed: #three) port: #entity) value equals: #(10 30).
	((browser paneNamed: #one) port: #selection) value: 5.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 5.
	((browser paneNamed: #two) port: #selection) value: #(1 3).
	self assert: ((browser paneNamed: #two) port: #selection) value equals: #(1 3).
	self assert: ((browser paneNamed: #three) port: #entity) value equals: #(1 3)