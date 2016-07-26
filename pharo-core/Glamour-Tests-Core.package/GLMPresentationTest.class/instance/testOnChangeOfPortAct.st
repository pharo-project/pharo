tests
testOnChangeOfPortAct
	| pane presentation timesTriggered |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	timesTriggered := 0.
	presentation 
		onChangeOfPort: #test 
		act: [ timesTriggered := timesTriggered + 1 ].
	(pane port: #test) value: 42.	
	self assert: timesTriggered equals: 1