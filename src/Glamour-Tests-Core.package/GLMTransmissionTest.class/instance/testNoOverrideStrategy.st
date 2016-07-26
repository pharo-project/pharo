tests
testNoOverrideStrategy
	| pane1 pane2 transmission presentation |
	pane1 := GLMPane named: 'pane1'.
	pane2 := GLMPane named: 'pane2'.
	transmission := GLMTransmission new.
	transmission ensurePresentIfNoneStrategy.
	transmission addActiveOrigin: (pane1 port: #port1).
	transmission destination: (pane2 port: #port2).
	transmission addPresentation: GLMPresentation new.
	self assert: transmission presentations size equals: 1.
	self assert: transmission destination pane == pane2.
	self assert: transmission transmissionStrategy transmission == transmission.
	(pane1 port: #port1) value: 1.
	transmission transmit.
	self assert: pane2 presentations size equals: 1.
	presentation := pane2 presentations first.
	self assert: transmission destination pane == pane2.
	(pane1 port: #port1) value: 2.
	transmission transmit.
	self assert: pane2 presentations first rootPrototype equals: presentation rootPrototype.
	self assert: pane2 presentations first == presentation