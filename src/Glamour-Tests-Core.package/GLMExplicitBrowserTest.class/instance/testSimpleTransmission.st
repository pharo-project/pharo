tests
testSimpleTransmission
	| browser pane1 pane2 presentation1 presentation2 transmission announcement |
	browser := GLMExplicitBrowser new.
	presentation1 := GLMPresentation new.
	presentation2 := GLMPresentation new.
	pane1 := browser addNewPane.
	pane2 := browser addNewPane.
	pane1 addPresentation: presentation1.
	pane2 addPresentation: presentation2.
	transmission := (GLMMockSimpleTransmission new)
		addActiveOrigin: (pane1 port: #selection);
		destination: (pane2 port: #entity);
		yourself.
	browser addTransmission: transmission.
	announcement := nil.
	presentation2 when: GLMContextChanged do: [ :ann | announcement := ann ].
	presentation1 selection: 'foo'.
	self assert: transmission wasTransmited.	"self assert: (pane1 port: #selection) seenTransmissions size = 0. 
	self assert: (pane2 port: #entity) seenTransmissions size = 1."	"self 
		assert: (pane1 port: #selection) seenTransmissions first ~~ transmission. "	"self assert: (pane2 port: #entity) seenTransmissions first == transmission. "
	self assert: (pane1 port: #selection) value equals: 'foo'.
	self assert: (pane2 port: #entity) value equals: 'foo'.
	self assert: announcement notNil