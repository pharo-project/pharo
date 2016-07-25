tests
testStatusbar
	| browser |
	browser := GLMTabulator withStatusbar.
	browser row: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list display: #(1 2 3) ].
	self assert: browser transmissions size equals: 2.
	browser startOn: 42