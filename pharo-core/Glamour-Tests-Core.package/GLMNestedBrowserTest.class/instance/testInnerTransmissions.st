tests
testInnerTransmissions
	"This tests that the nested transmissions have the destination correctly set 
	to be in the pane from the outer browser."

	| browser1 browser2 presentation innerBrowser |
	browser1 := GLMExplicitBrowser new.
	browser2 := GLMExplicitBrowser new.
	presentation := GLMPresentation new.
	browser1 addNewPane name: 'pane1'.
	browser2 addNewPane name: 'innerpane 1'.	"Jumpstart presentation for outer browser."
	browser1
		addTransmission:
			((GLMTransmission new)
				ensureReplacePresentationsStrategy;
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser1;
								name: #entity;
								yourself);
				destination: (browser1 panes first port: #entity);
				addPresentation: browser2;
				yourself).	"Jumpstart presentation for inner browser."
	browser2
		addTransmission:
			((GLMTransmission new)
				ensureReplacePresentationsStrategy;
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser2;
								name: #entity;
								yourself);
				destination: (browser2 panes first port: #entity);
				addPresentation: presentation;
				yourself).
	(browser1 pane port: #entity) value: 42.
	innerBrowser := browser1 panes first presentations first.
	self assert: innerBrowser transmissions size equals: 1.
	self assert: (innerBrowser transmissions first origins first isKindOf: GLMPresentationBoundPort).
	self assert: innerBrowser transmissions first origins first pane == browser1 panes first