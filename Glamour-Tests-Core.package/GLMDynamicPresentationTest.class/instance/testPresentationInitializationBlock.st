tests
testPresentationInitializationBlock
	| dynamic count |
	count := 0.
	dynamic := GLMDynamicPresentation new.
	dynamic transformation: [ :entity | 
		GLMListPresentation new 
			initialize: [ :pres | 
				count := count +1.
				pres selection: 5 ] ].
	dynamic entity: (1 to: 42).
	self assert: count = 0.
	self assert: (dynamic currentPresentation presentations first selection = 5).
	self assert: count = 1