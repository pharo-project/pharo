tests
testTransmissionBundle
	| browser pane1 pane2 pane3 presentation1 presentation2 transmission |
	browser := GLMExplicitBrowser new.
	presentation1 := GLMPresentation new.
	presentation2 := GLMPresentation new.
	pane1 := GLMTestPane new.
	pane2 := GLMTestPane new.
	pane3 := GLMTestPane new.
	pane1 addPresentation: GLMPresentation new.
	pane2 addPresentation: GLMPresentation new.
	pane3 addPresentation: presentation1.
	transmission := (GLMTransmission new)
		ensureReplacePresentationsStrategy;
		addActiveOrigin: (pane1 port: #selection);
		addPassiveOrigin: (pane2 port: #selection);
		destination: (pane3 port: #entity);
		addPresentation: presentation2;
		yourself.
	browser addPane: pane1.
	browser addPane: pane2.
	browser addPane: pane3.
	browser addTransmission: transmission.
	self assert: pane3 presentations size equals: 1.
	self assert: pane3 presentations first == presentation1.
	pane2 presentations first selection: #foo.
	self assert: (pane2 port: #selection) seenTransmissions size equals: 1.
	self assert: (pane3 port: #entity) seenTransmissions size equals: 0.
	pane1 presentations first selection: #bar.
	self assert: (pane1 port: #selection) seenTransmissions size equals: 1.
	self assert: (pane3 port: #entity) seenTransmissions size equals: 1.
	self assert: (pane3 port: #entity) seenTransmissions last == transmission.
	self assert: (pane3 port: #entity) value equals: (GLMMultiValue withAll: #(#bar #foo)).
	self assert: pane3 presentations size equals: 1.
	self assert: pane3 presentations first rootPrototype equals: presentation2