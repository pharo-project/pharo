tests
testPortInitializationDoesNotAffectTransmissionPort
	| browser count |
	count := 0.
	browser := GLMTabulator new.
	browser
		column: #one.
	(browser transmit)
		fromOutsidePort: #entity;
		to: #one port: #entity;
		andShow: [ :a :entity | 
			a list initialize: [ :pres |  
				count := count + 1.
				pres 
					entity: (1 to: 10) ] ].
	self assert: (browser panes first port: #entity) value equals: nil.
	browser startOn: (1 to: 42).
	self assert: (browser panes first port: #entity) value equals: (1 to: 42).
	self assert: count = 1.
	(browser pane port: #entity) value: (1 to: 43).
	self assert: (browser panes first port: #entity) value equals: (1 to: 43).
	self assert: count = 2.
