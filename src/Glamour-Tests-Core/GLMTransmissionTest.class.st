Class {
	#name : #GLMTransmissionTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMTransmissionTest >> testBundleStrategy [
	| pane1 pane2 transmission presentation |
	pane1 := GLMPane named: 'pane1'.
	pane2 := GLMPane named: 'pane2'.
	transmission := GLMTransmission new.
	transmission ensureReplacePresentationsStrategy.	"ensureNoOverrideBundleStrategy"
	transmission addActiveOrigin: (pane1 port: #port1).
	transmission destination: (pane2 port: #port2).
	transmission addPresentation: GLMPresentation new.
	(pane1 port: #port1) value: 1.
	transmission transmit.
	self assert: pane2 presentations size equals: 1.
	presentation := pane2 presentations first.
	(pane1 port: #port1) value: 2.
	transmission transmit.
	self assert: pane2 presentations first rootPrototype equals: presentation rootPrototype.
	self assert: pane2 presentations first ~~ presentation
]

{ #category : #tests }
GLMTransmissionTest >> testCopy [
	| transmission port1 port2 newTransmission |
	transmission := GLMTransmission new.
	port1 := GLMSimplePort new.
	port2 := GLMSimplePort new.
	transmission destination: port2.
	transmission addActiveOrigin: port1.
	newTransmission := transmission copy.
	self assert: newTransmission destination ~~ transmission destination.
	self assert: newTransmission origins first ~~ transmission origins first.
]

{ #category : #tests }
GLMTransmissionTest >> testDefaultStrategy [
	self assert: GLMTransmission new transmissionStrategy class equals: GLMNoStrategy
]

{ #category : #tests }
GLMTransmissionTest >> testNoOverrideStrategy [
	| pane1 pane2 transmission presentation |
	pane1 := GLMPane named: 'pane1'.
	pane2 := GLMPane named: 'pane2'.
	transmission := GLMTransmission new.
	transmission ensurePresentIfNoneStrategy.
	transmission addActiveOrigin: (pane1 port: #port1).
	transmission destination: (pane2 port: #port2).
	transmission addPresentation: GLMPresentation new.
	self assert: transmission presentations size equals: 1.
	self assert: transmission destination pane identicalTo: pane2.
	self assert: transmission transmissionStrategy transmission identicalTo: transmission.
	(pane1 port: #port1) value: 1.
	transmission transmit.
	self assert: pane2 presentations size equals: 1.
	presentation := pane2 presentations first.
	self assert: transmission destination pane identicalTo: pane2.
	(pane1 port: #port1) value: 2.
	transmission transmit.
	self assert: pane2 presentations first rootPrototype equals: presentation rootPrototype.
	self assert: pane2 presentations first identicalTo: presentation
]

{ #category : #tests }
GLMTransmissionTest >> testSimple [
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
]

{ #category : #tests }
GLMTransmissionTest >> testTriggerConditions [ 
	"Tests that the transmission is triggered only when the origin changes." 
	
	| transmission port1 port2 | 
	transmission := GLMTransmission new. 
	port1 := GLMSimplePort new name: #port1. 
	port2 := GLMSimplePort new name: #port2. 
	transmission addActiveOrigin: port1. 
	transmission destination: port2. 
	self assert: (transmission originatesAt: port1). 
	self assert: (transmission originatesAt: port2) not. 
	self assert: (transmission originatesAt: GLMPort new) not
]

{ #category : #tests }
GLMTransmissionTest >> testTriggerConditionsWithParentPort [ 
	| transmission presentation | 
	transmission := GLMTransmission new. 
	presentation := GLMPresentation new. 
	transmission addActiveOrigin: 
			((GLMPresentationBoundPort new) 
				presentation: presentation; 
				name: #port1; 
				yourself). 
	self assert: (transmission originatesAt: 
					((GLMPresentationBoundPort new) 
						presentation: presentation; 
						name: #port1; 
						yourself)). 
	self assert: (transmission originatesAt: (presentation pane port: #port1))
]

{ #category : #tests }
GLMTransmissionTest >> testWhenCondition [
	"Tests that the transmission is triggered only when the origin changes."

	| transmission port1 port2 |
	transmission := GLMTransmission new.
	port1 := (GLMSimplePort new)
		name: #port1;
		value: #value1.
	port2 := GLMSimplePort new name: #port2.
	transmission addActiveOrigin: port1.
	transmission destination: port2.
	transmission condition: [ :entity | entity = #value1 ].
	self assert: transmission meetsCondition.
	transmission transmit.
	self assert: port2 value equals: #value1
]
