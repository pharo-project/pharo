Class {
	#name : #GLMBundleTransmissionTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMBundleTransmissionTest >> testCopy [
	| transmission newTransmission | 
	transmission := GLMTransmission new. 
	transmission addActiveOrigin: ((GLMSimplePort new) name: #port1; value: 1). 
	transmission addPassiveOrigin: ((GLMSimplePort new) name: #port2; value: 2). 
	transmission addActiveOrigin: ((GLMSimplePort new) name: #port3; value: 3). 
	transmission addPassiveOrigin: ((GLMSimplePort new) name: #port4; value: 4). 
	transmission destination: (GLMSimplePort new name: #port5). 

	newTransmission := transmission copy.
	
	self assert: transmission originReferences first ~= newTransmission originReferences first.
]

{ #category : #tests }
GLMBundleTransmissionTest >> testPorts [
	"Tests that the origins are set up properly and that their order is retained."

	| transmission |
	transmission := GLMTransmission new.
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
	transmission
		addActiveOrigin:
			((GLMSimplePort new)
				name: #port3;
				value: 3).
	transmission
		addPassiveOrigin:
			((GLMSimplePort new)
				name: #port4;
				value: 4).
	transmission destination: (GLMSimplePort new name: #port5).
	self assert: transmission originReferences size equals: 4.
	transmission originReferences do: [ :each | self assert: (each isKindOf: GLMOriginPortReference) ].
	self assert: (transmission originReferences at: 1) port name equals: #port1.
	self assert: (transmission originReferences at: 2) port name equals: #port2.
	self assert: (transmission originReferences at: 3) port name equals: #port3.
	self assert: (transmission originReferences at: 4) port name equals: #port4.
	self assert: transmission origins size equals: 4.
	transmission origins do: [ :each | self assert: (each isKindOf: GLMSimplePort) ].
	self assert: (transmission origins at: 1) name equals: #port1.
	self assert: (transmission origins at: 2) name equals: #port2.
	self assert: (transmission origins at: 3) name equals: #port3.
	self assert: (transmission origins at: 4) name equals: #port4.
	self assert: transmission activeOrigins size equals: 2.
	transmission activeOrigins do: [ :each | self assert: (each isKindOf: GLMSimplePort) ].
	self assert: (transmission activeOrigins at: 1) name equals: #port1.
	self assert: (transmission activeOrigins at: 2) name equals: #port3.
	self assert: transmission passiveOrigins size equals: 2.
	transmission passiveOrigins do: [ :each | self assert: (each isKindOf: GLMSimplePort) ].
	self assert: (transmission passiveOrigins at: 1) name equals: #port2.
	self assert: (transmission passiveOrigins at: 2) name equals: #port4.
	self assert: (transmission destination isKindOf: GLMSimplePort).
	self assert: transmission destination name equals: #port5
]

{ #category : #tests }
GLMBundleTransmissionTest >> testPresentations [
	"Tests that the presentations are set up correctly."

	| transmission presentation1 presentation2 presentation3 |
	transmission := GLMTransmission new ensureReplacePresentationsStrategy.
	transmission
		addActiveOrigin:
			(GLMSimplePort new
				name: #port1;
				value: 1).
	transmission
		addPassiveOrigin:
			(GLMSimplePort new
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
	self assert: (transmission transmissionStrategy presentations at: 1) title equals: presentation1 title.
	self assert: (transmission transmissionStrategy presentations at: 2) title equals: presentation2 title.
	self assert: (transmission transmissionStrategy presentations at: 3) title equals: presentation3 title.
	self assert: (transmission transmissionStrategy presentations at: 1) ~~ presentation1.
	self assert: (transmission transmissionStrategy presentations at: 2) ~~ presentation2.
	self assert: (transmission transmissionStrategy presentations at: 3) ~~ presentation3
]

{ #category : #tests }
GLMBundleTransmissionTest >> testTransmitting [
	"Tests that the origins are set up properly and that their order is retained."

	| transmission |
	transmission := GLMTransmission new.
	transmission
		addActiveOrigin:
			((GLMSimplePort new)
				name: #port1;
				value: 1).
	transmission destination: (GLMSimplePort new name: #port5).
	self assert: transmission value equals: 1.
	self assert: transmission destination value isNil.
	transmission transmit.
	self assert: transmission destination value equals: 1.
	transmission
		addPassiveOrigin:
			((GLMSimplePort new)
				name: #port2;
				value: 2).
	transmission
		addActiveOrigin:
			((GLMSimplePort new)
				name: #port3;
				value: 3).
	transmission
		addPassiveOrigin:
			((GLMSimplePort new)
				name: #port4;
				value: 4).
	self assert: transmission value equals: (GLMMultiValue withAll: #(1 2 3 4)).
	transmission transmit.
	self assert: transmission destination value equals: (GLMMultiValue withAll: #(1 2 3 4))
]

{ #category : #tests }
GLMBundleTransmissionTest >> testTriggerConditions [ 
	"Tests that the transmission is triggered only when the active origins change." 
	 
	| transmission port1 port2 | 
	transmission := GLMTransmission new. 
	port1 := GLMSimplePort new name: #port1. 
	port2 := GLMSimplePort new name: #port2. 
	transmission addActiveOrigin: port1. 
	transmission addPassiveOrigin: port2. 
	transmission destination: (GLMPort new name: #port3). 
	self assert: (transmission originatesAt: port1). 
	self assert: (transmission originatesAt: port2) not
]

{ #category : #tests }
GLMBundleTransmissionTest >> testTriggerConditionsWithParentPort [ 
	 
	| transmission presentation | 
	transmission := GLMTransmission new. 
	presentation := GLMPresentation new. 
	transmission 
		addActiveOrigin: 
			((GLMPresentationBoundPort new) 
				presentation: presentation; 
				name: #port1; 
				yourself). 
	self 
		assert: 
			(transmission 
				originatesAt: 
					((GLMPresentationBoundPort new) 
						presentation: presentation; 
						name: #port1; 
						yourself)). 
	self assert: (transmission originatesAt: (presentation pane port: #port1))
]
