tests
testTriggeringConditions
	| browser pane1 pane2 transmission |
	browser := GLMExplicitBrowser new.
	browser pane: GLMTestPane new.
	pane1 := GLMTestPane new.
	pane2 := GLMTestPane new.
	transmission := (GLMTransmission new)
		addActiveOrigin: (pane1 port: #selection);
		destination: (pane2 port: #entity);
		yourself.
	browser addPane: pane1.
	browser addPane: pane2.
	browser addTransmission: transmission.
	self assert: (pane1 port: #selection) seenTransmissions isEmpty.
	self assert: (pane2 port: #entity) seenTransmissions isEmpty.
	(pane1 port: #selection) value: 12.
	self assert: (pane1 port: #selection) seenTransmissions size equals: 0.	"self 
		assert: 
			((pane1 port: #selection) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). "
	self assert: (pane2 port: #entity) seenTransmissions size equals: 1.
	self assert: (pane2 port: #entity) seenTransmissions first == transmission.	"Setting the same value should re-publish but not trigger inner transmissions."
	(pane1 port: #selection) value: 12.
	self assert: (pane1 port: #selection) seenTransmissions size equals: 0.	"self 
		assert: 
			((pane1 port: #selection) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). "	"self 
		assert: 
			((pane1 port: #selection) seenTransmissions last isKindOf: 
					GLMSimpleTransmission). "
	self assert: (pane2 port: #entity) seenTransmissions size equals: 1.
	self assert: (pane2 port: #entity) seenTransmissions first == transmission