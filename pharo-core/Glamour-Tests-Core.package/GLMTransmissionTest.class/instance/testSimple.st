tests
testSimple
	| pane1 pane2 transmission |
	pane1 := GLMPane named: 'test1'.
	pane2 := GLMPane named: 'test2'.
	transmission := (GLMTransmission new)
		addActiveOrigin: (pane1 port: #output);
		destination: (pane2 port: #input);
		yourself.
	(pane1 port: #output) value: 42.
	self assert: (pane1 port: #output) value equals: 42.
	self assert: (pane2 port: #input) value equals: nil.
	transmission transmit.
	self assert: (pane1 port: #output) value equals: 42.
	self assert: (pane2 port: #input) value equals: 42