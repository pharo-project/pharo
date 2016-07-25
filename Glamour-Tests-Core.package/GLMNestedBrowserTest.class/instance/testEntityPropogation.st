tests
testEntityPropogation
	"This tests that the nested transmissions have the destination correctly set 
	to be in the pane from the outer browser."

	| browser1 browser2 presentation innerBrowser transmission1 transmission2 |
	browser1 := GLMExplicitBrowser new.
	browser2 := GLMExplicitBrowser new.
	presentation := GLMPresentation new.
	browser1 title: 'browser1'.
	browser2 title: 'browser2'.
	browser1 addNewPane name: 'pane1'.
	browser2 addNewPane name: 'innerpane 1'.	"Jumpstart presentation for outer browser."
	transmission1 := GLMTransmission new ensureReplacePresentationsStrategy.
	transmission1 transmissionStrategy addPresentation: browser2.
	browser1
		addTransmission:
			(transmission1
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser1;
								name: #entity;
								yourself);
				destination: (browser1 panes first port: #entity);
				yourself).	"Jumpstart presentation for inner browser."
	transmission2 := GLMTransmission new ensureReplacePresentationsStrategy.
	transmission2 transmissionStrategy addPresentation: presentation.
	browser2
		addTransmission:
			(transmission2
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser2;
								name: #entity;
								yourself);
				destination: (browser2 panes first port: #entity);
				yourself).
	self assert: (browser1 pane port: #entity) value isNil.
	self assert: browser1 panes size equals: 1.
	self assert: (browser1 panes first port: #entity) value isNil.
	self assert: browser1 panes first presentations isEmpty.
	self assert: browser2 panes size equals: 1.
	self assert: (browser2 panes first port: #entity) value isNil.
	self assert: browser2 panes first presentations isEmpty.
	(browser1 pane port: #entity) value: 42.
	self assert: (browser1 pane port: #entity) value equals: 42.
	self assert: browser1 panes size equals: 1.
	self assert: (browser1 panes first port: #entity) value equals: 42.
	self assert: browser1 panes first presentations size equals: 1.
	innerBrowser := browser1 panes first presentations first.
	self assert: innerBrowser rootPrototype equals: browser2.
	self assert: innerBrowser pane == browser1 panes first.
	self assert: innerBrowser panes size equals: 1.
	self assert: (innerBrowser panes first port: #entity) value equals: 42.
	self assert: innerBrowser panes first presentations size equals: 1.
	self assert: innerBrowser panes first presentations first rootPrototype equals: presentation