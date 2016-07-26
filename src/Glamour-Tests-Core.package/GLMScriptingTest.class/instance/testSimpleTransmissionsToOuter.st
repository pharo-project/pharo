tests
testSimpleTransmissionsToOuter
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser sendTo: #outer -> #increased from: #outer -> #entity with: [ :x | x + 1 ].
	(browser transmit)
		to: #one;
		from: #outer port: #increased;
		andShow: [ :a | a text ].
	browser startOn: 41.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 42