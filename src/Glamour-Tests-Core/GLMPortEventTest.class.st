Class {
	#name : #GLMPortEventTest,
	#superclass : #TestCase,
	#category : #Glamour-Tests-Core
}

{ #category : #tests }
GLMPortEventTest >> testCreation [
	| anEvent portOne portTwo |
	portOne := (GLMSimplePort new) name: #portOne; value: 1.
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	anEvent := GLMPortEvent on: portOne previouslyValued: portTwo.
	self assert: anEvent port equals: portOne.
	self assert: anEvent oldValue equals: portTwo.
	self assert: anEvent transmissionContext isEmpty
]

{ #category : #tests }
GLMPortEventTest >> testCreationWithTransmissionContext [
	| anEvent aContext portOne portTwo |
	aContext := OrderedCollection new.
	portOne := (GLMSimplePort new) name: #portOne; value: 1.
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	anEvent := GLMPortEvent on: portOne previouslyValued: portTwo in: aContext.
	self assert: anEvent port equals: portOne.
	self assert: anEvent oldValue equals: portTwo.
	self assert: anEvent transmissionContext equals: aContext
]

{ #category : #tests }
GLMPortEventTest >> testInterdependentPanesCycleBreak [
	| browser outerCount oneCount |
	outerCount := 0.
	oneCount := 0.
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	browser
		transmit to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x * 2 ] ].
	browser
		transmit to: #three;
		from: #two;
		andShow: [ :a |  
					(a list)
						display: [ :x | 1 to: x * 3 ];
						populate: #focus
							on: $f
							entitled: 'Focus'
							with: [ :list | list selection ] ].
	browser transmit toOutsidePort: #focus; from: #three port: #focus; transformed: [ :x | outerCount := outerCount + 1 ].
	browser transmit to: #one port: #selection; fromOutsidePort: #focus; transformed: [ :x | oneCount := oneCount + 1 ].
	browser startOn: 5.
	((browser paneNamed: #one) port: #selection) value: 3.
	((browser paneNamed: #two) port: #selection) value: 6.
	((browser paneNamed: #three) port: #selection) value: 12.
	((browser paneNamed: #three) port: #focus) value: 12.
	self assert: oneCount equals: 1.
	self assert: outerCount equals: 1
]

{ #category : #tests }
GLMPortEventTest >> testInterdependentPanesProperPorts [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	browser
		transmit to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x ] ].
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a list display: [ :x | 1 to: x * 2 ] ].
	browser
		transmit to: #three;
		from: #two;
		andShow: [ :a | 
					(a list)
						display: [ :x | 1 to: x * 3 ];
						populate: #focus
							on: $f
							entitled: 'Focus'
							with: [ :list | list selection ] ].
	browser transmit toOutsidePort: #focus; from: #three port: #focus.
	browser transmit to: #one port: #selection; fromOutsidePort: #focus; transformed: [ :x | x + 1 ].
	browser transmit to: #two port: #selection; fromOutsidePort: #focus; transformed: [ :x | x + 2 ].
	browser startOn: 5.
	((browser paneNamed: #one) port: #selection) value: 3.
	((browser paneNamed: #two) port: #selection) value: 6.
	((browser paneNamed: #three) port: #selection) value: 12.
	((browser paneNamed: #three) port: #focus) value: 12.
	self assert: (browser pane port: #focus) value equals: 12.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 5.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 13.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 13.
	self assert: ((browser paneNamed: #two) port: #selection) value equals: 14.
	self assert: ((browser paneNamed: #three) port: #entity) value equals: 14.
	self assert: ((browser paneNamed: #three) port: #selection) value equals: nil
]

{ #category : #tests }
GLMPortEventTest >> testPortName [
	| anEvent portTwo|
	portTwo := (GLMSimplePort new) name: #portTwo; value: 2.
	anEvent := GLMPortEvent
		on:
			((GLMPort new)
				name: '1';
				yourself)
		previouslyValued: portTwo.
	self assert: anEvent portName equals: '1'
]
