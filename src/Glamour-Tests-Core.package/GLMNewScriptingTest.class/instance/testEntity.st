tests
testEntity
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list ].
	browser startOn: ($a to: $d).
	self assert: ((browser paneNamed: #one) port: #entity) value equals: ($a to: $d)