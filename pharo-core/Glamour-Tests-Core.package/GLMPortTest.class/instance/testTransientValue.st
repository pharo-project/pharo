tests
testTransientValue
	| port pane browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	(browser transmit)
		fromOutsidePort: #entity;
		to: #one port: #entity.
	(browser transmit)
		from: #one port: #entity;
		to: #two;
		andShow: [ :a | a text ].
	browser startOn: 42.
	self assert: (browser panes first port: #entity) value equals: 42.
	self assert: (browser panes last port: #entity) value equals: 42.
	(browser panes first port: #entity) transientValue: 43.
	self assert: (browser pane port: #entity) value equals: 42.
	self assert: (browser panes first port: #entity) value equals: 42.
	self assert: (browser panes last port: #entity) value equals: 43.
	(browser pane port: #entity) transientValue: 44.
	self assert: (browser pane port: #entity) value equals: 42.
	self assert: (browser panes first port: #entity) value equals: 44.
	self assert: (browser panes last port: #entity) value equals: 44