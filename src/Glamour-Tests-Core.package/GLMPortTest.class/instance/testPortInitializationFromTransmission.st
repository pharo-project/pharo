tests
testPortInitializationFromTransmission
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one.
	(browser transmit)
		fromOutsidePort: #entity;
		to: #one port: #entity;
		andShow: [ :a :entity | 
			a text initialize: [ :pres |  
				pres 
					text: ('foo', entity asString);
					selectionInterval: (2 to: 3) ] ].
	self assert: (browser panes first port: #text) value equals: nil.
	self assert: (browser panes first port: #selectionInterval) value equals: nil.
	browser startOn: 42.
	self assert: (browser panes first port: #text) value equals: 'foo42'.
	self assert: (browser panes first port: #selectionInterval) value equals: (2 to: 3).
	(browser pane port: #entity) transientValue: 43.
	self assert: (browser panes first port: #text) value equals: 'foo43'.
	self assert: (browser panes first port: #selectionInterval) value equals: (2 to: 3).
	(browser pane port: #entity) value: 44.
	self assert: (browser panes first port: #text) value equals: 'foo44'.
	self assert: (browser panes first port: #selectionInterval) value equals: (2 to: 3).