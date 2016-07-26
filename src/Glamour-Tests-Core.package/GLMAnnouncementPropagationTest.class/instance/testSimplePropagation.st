tests
testSimplePropagation
	| browser |
	browser := GLMTabulator new.
	browser column: #pane1.
	(browser transmit)
		to: #pane1;
		andShow: [ :a | a text ].
	browser startOn: 42.
	self assert: GLMLogger instance announcements copy size equals: 8