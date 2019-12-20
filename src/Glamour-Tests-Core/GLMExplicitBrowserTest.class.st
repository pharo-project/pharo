Class {
	#name : #GLMExplicitBrowserTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMExplicitBrowserTest >> testAddTransmissionReturn [
	| transmission |
	transmission := GLMTransmission new.
	self assert: (GLMExplicitBrowser new addTransmission: transmission) identicalTo: transmission
]

{ #category : #tests }
GLMExplicitBrowserTest >> testJumpstartTransmission [
	| browser presentation |
	browser := GLMExplicitBrowser new.
	presentation := GLMPresentation new.
	browser addNewPane.
	browser
		addTransmission:
			((GLMTransmission new)
				ensureReplacePresentationsStrategy;
				addActiveOrigin: (browser pane port: #entity);
				destination: (browser panes first port: #entity);
				addPresentation: presentation;
				yourself).
	self assert: (browser panes first port: #entity) value isNil.
	self assertEmpty: browser panes first presentations.
	(browser pane port: #entity) value: #foo.
	self assert: (browser panes first port: #entity) value equals: #foo.
	self assert: browser panes first presentations size equals: 1.
	self assert: browser panes first presentations first rootPrototype equals: presentation
]

{ #category : #tests }
GLMExplicitBrowserTest >> testPresentationBoundPort [
	| browser outerPort1 outerPort2 transmission1 transmission2 |
	browser := GLMExplicitBrowser new.
	browser addNewPane.
	GLMTestPane new addPresentation: browser.
	outerPort1 := (GLMPresentationBoundPort new)
		presentation: browser;
		name: #outerPort1;
		yourself.
	outerPort2 := (GLMPresentationBoundPort new)
		presentation: browser;
		name: #outerPort2;
		yourself.	"Capturing"
	browser
		addTransmission:
			(transmission1 := (GLMMockSimpleTransmission new)
				addActiveOrigin: outerPort1;
				destination: (browser panes first port: #innerPort1);
				yourself).	"Publishing"
	browser
		addTransmission:
			((transmission2 := GLMMockSimpleTransmission new)
				addActiveOrigin: (browser panes first port: #innerPort2);
				destination: outerPort2;
				yourself).
	(browser pane port: #outerPort1) value: 42.
	self assert: (browser panes first port: #innerPort1) value equals: 42.
	self assert: transmission1 wasTransmited.
	self deny: transmission2 wasTransmited.	"self assert: (browser pane port: #outerPort1) seenTransmissions size = 0. "	"self 
		assert: 
			((browser pane port: #outerPort1) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). 
	self 
		assert: 
			((browser pane port: #outerPort1) seenTransmissions first origin isKindOf: 
					GLMSimplePort). 
	self 
		assert: 
			(browser pane port: #outerPort1) seenTransmissions first destination 
				== (browser pane port: #outerPort1). "	"self 
		assert: 
			(browser panes first port: #innerPort1) seenTransmissions size = 1. 
	self 
		assert: 
			(browser panes first port: #innerPort1) seenTransmissions first 
				== browser transmissions first. "
	(browser panes first port: #innerPort2) value: 43.
	self assert: (browser pane port: #outerPort2) value equals: 43	"self 
		assert: 
			(browser panes first port: #innerPort2) seenTransmissions size = 0. "	"self 
		assert: 
			((browser panes first port: #innerPort2) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). 
	self 
		assert: 
			((browser panes first port: #innerPort2) seenTransmissions first origin 
				isKindOf: GLMSimplePort). 
	self 
		assert: 
			(browser panes first port: #innerPort2) seenTransmissions first destination 
				== (browser panes first port: #innerPort2). "	"self assert: (browser pane port: #outerPort2) seenTransmissions size = 1. 
	self 
		assert: 
			(browser pane port: #outerPort2) seenTransmissions first 
				== browser transmissions last"
]

{ #category : #tests }
GLMExplicitBrowserTest >> testSetup [
	| browser pane1 pane2 transmission |
	browser := GLMExplicitBrowser new.
	pane1 := browser addNewPane.
	pane2 := browser addNewPane.
	transmission := GLMTransmission new
		addActiveOrigin: (pane1 port: #selection);
		destination: (pane2 port: #entity);
		yourself.
	browser addTransmission: transmission.
	self assert: browser panes size equals: 2.
	self assert: browser transmissions size equals: 1.
	self assert: browser panes first identicalTo: pane1.
	self assert: browser panes last identicalTo: pane2.
	self assert: pane1 browser identicalTo: browser.
	self assert: pane2 browser identicalTo: browser.
	self assert: browser transmissions first identicalTo: transmission
]

{ #category : #tests }
GLMExplicitBrowserTest >> testSimpleTransmission [
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
]

{ #category : #tests }
GLMExplicitBrowserTest >> testTransmissionBundle [
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
	transmission := GLMTransmission new
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
	self assert: pane3 presentations first identicalTo: presentation1.
	pane2 presentations first selection: #foo.
	self assert: (pane2 port: #selection) seenTransmissions size equals: 1.
	self assert: (pane3 port: #entity) seenTransmissions size equals: 0.
	pane1 presentations first selection: #bar.
	self assert: (pane1 port: #selection) seenTransmissions size equals: 1.
	self assert: (pane3 port: #entity) seenTransmissions size equals: 1.
	self assert: (pane3 port: #entity) seenTransmissions last identicalTo: transmission.
	self assert: (pane3 port: #entity) value equals: (GLMMultiValue withAll: #(#bar #foo)).
	self assert: pane3 presentations size equals: 1.
	self assert: pane3 presentations first rootPrototype equals: presentation2
]

{ #category : #tests }
GLMExplicitBrowserTest >> testTriggeringConditions [
	| browser pane1 pane2 transmission |
	browser := GLMExplicitBrowser new.
	browser pane: GLMTestPane new.
	pane1 := GLMTestPane new.
	pane2 := GLMTestPane new.
	transmission := GLMTransmission new
		addActiveOrigin: (pane1 port: #selection);
		destination: (pane2 port: #entity);
		yourself.
	browser addPane: pane1.
	browser addPane: pane2.
	browser addTransmission: transmission.
	self assertEmpty: (pane1 port: #selection) seenTransmissions.
	self assertEmpty: (pane2 port: #entity) seenTransmissions.
	(pane1 port: #selection) value: 12.
	self assertEmpty: (pane1 port: #selection) seenTransmissions.	"self 
		assert: 
			((pane1 port: #selection) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). "
	self assert: (pane2 port: #entity) seenTransmissions size equals: 1.
	self assert: (pane2 port: #entity) seenTransmissions first identicalTo: transmission.	"Setting the same value should re-publish but not trigger inner transmissions."
	(pane1 port: #selection) value: 12.
	self assertEmpty: (pane1 port: #selection) seenTransmissions.	"self 
		assert: 
			((pane1 port: #selection) seenTransmissions first isKindOf: 
					GLMSimpleTransmission). "	"self 
		assert: 
			((pane1 port: #selection) seenTransmissions last isKindOf: 
					GLMSimpleTransmission). "
	self assert: (pane2 port: #entity) seenTransmissions size equals: 1.
	self assert: (pane2 port: #entity) seenTransmissions first identicalTo: transmission
]
