tests
testPresentations
	"Tests that the presentations are set up correctly."

	| transmission presentation1 presentation2 presentation3 |
	transmission := GLMTransmission new ensureReplacePresentationsStrategy.
	transmission
		addActiveOrigin:
			((GLMSimplePort new)
				name: #port1;
				value: 1).
	transmission
		addPassiveOrigin:
			((GLMSimplePort new)
				name: #port2;
				value: 2).
	transmission destination: (GLMSimplePort new name: #port3).
	presentation1 := GLMPresentation new title: 'presentation1'.
	presentation2 := GLMPresentation new title: 'presentation2'.
	presentation3 := GLMPresentation new title: 'presentation3'.
	transmission transmissionStrategy addPresentation: presentation1.
	transmission transmissionStrategy addPresentation: presentation2.
	transmission transmissionStrategy addPresentation: presentation3.	"Test correct setup"
	self assert: transmission transmissionStrategy presentations size equals: 3.
	self assert: (transmission transmissionStrategy presentations at: 1) title = presentation1 title.
	self assert: (transmission transmissionStrategy presentations at: 2) title = presentation2 title.
	self assert: (transmission transmissionStrategy presentations at: 3) title = presentation3 title.
	self assert: (transmission transmissionStrategy presentations at: 1) ~~ presentation1.
	self assert: (transmission transmissionStrategy presentations at: 2) ~~ presentation2.
	self assert: (transmission transmissionStrategy presentations at: 3) ~~ presentation3