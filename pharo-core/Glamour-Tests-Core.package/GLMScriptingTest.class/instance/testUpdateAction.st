tests
testUpdateAction
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	(browser transmit)
		to: #one;
		andShow: [ :a | a list populate: #foo on: $m with: [ :list :input | 42 ] ].
	(browser transmit)
		to: #two;
		from: #one port: #foo;
		andShow: [ :a | a text ].
	browser startOn: #(1 2 3).
	self assert: browser panes first presentations size equals: 1.
	self assert: browser panes last presentations isEmpty.
	browser panes first presentations first actions first actOn: browser panes first presentations first.
	self assert: browser panes second presentations size equals: 1.
	self assert: browser panes second presentations first entity equals: 42