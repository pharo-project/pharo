Class {
	#name : #GLMPortTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMPortTest >> testPanePortTransmissionCallingReceiveIn [
	| originPort destinationPort transmission |
	originPort := GLMTestPane new port: #originPort.
	destinationPort := GLMTestPane new port: #destinationPort.
	self assert: destinationPort seenTransmissions isEmpty.
	transmission := (GLMTransmission new)
		addActiveOrigin: originPort;
		destination: destinationPort;
		transformation: [ :x | x + 1 ].
	originPort value: 41.
	transmission transmit.
	self assert: destinationPort seenTransmissions size equals: 1.
	self assert: destinationPort seenTransmissions first origins first value equals: 41.
	self assert: destinationPort seenTransmissions first destination == destinationPort.
	self assert: destinationPort value equals: 42
]

{ #category : #tests }
GLMPortTest >> testPortEquality [
	| presentation port |
	presentation := GLMPresentation new.
	port := (GLMPresentationBoundPort new)
		name: #port;
		presentation: presentation.
	self assert: port equals: (presentation pane port: #port).
	self assert: (presentation pane port: #port) equals: port.
	self assert: port hash equals: (presentation pane port: #port) hash.
	self assert: (presentation pane port: #port) hash equals: port hash
]

{ #category : #tests }
GLMPortTest >> testPortInitializationDoesNotAffectTransmissionPort [
	| browser count |
	count := 0.
	browser := GLMTabulator new.
	browser
		column: #one.
	(browser transmit)
		fromOutsidePort: #entity;
		to: #one port: #entity;
		andShow: [ :a :entity | 
			a list initialize: [ :pres |  
				count := count + 1.
				pres 
					entity: (1 to: 10) ] ].
	self assert: (browser panes first port: #entity) value equals: nil.
	browser startOn: (1 to: 42).
	self assert: (browser panes first port: #entity) value equals: (1 to: 42).
	self assert: count = 1.
	(browser pane port: #entity) value: (1 to: 43).
	self assert: (browser panes first port: #entity) value equals: (1 to: 43).
	self assert: count = 2.

]

{ #category : #tests }
GLMPortTest >> testPortInitializationFromTransmission [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one.
	(browser transmit)
		fromOutsidePort: #entity;
		to: #one port: #entity;
		andShow: [ :a :entity | 
			a text initialize: [ :pres |  
				pres 
					text: ('foo', entity asString);
					selectionInterval: (2 to: 3) ] ].
	self assert: (browser panes first port: #text) value equals: nil.
	self assert: (browser panes first port: #selectionInterval) value equals: nil.
	browser startOn: 42.
	self assert: (browser panes first port: #text) value equals: 'foo42'.
	self assert: (browser panes first port: #selectionInterval) value equals: (2 to: 3).
	(browser pane port: #entity) transientValue: 43.
	self assert: (browser panes first port: #text) value equals: 'foo43'.
	self assert: (browser panes first port: #selectionInterval) value equals: (2 to: 3).
	(browser pane port: #entity) value: 44.
	self assert: (browser panes first port: #text) value equals: 'foo44'.
	self assert: (browser panes first port: #selectionInterval) value equals: (2 to: 3).
]

{ #category : #tests }
GLMPortTest >> testPresentationBoundPort [
	| presentation port |
	presentation := GLMPresentation new.
	port := (GLMPresentationBoundPort new)
		name: #port;
		presentation: presentation.
	port value: 42.
	self assert: (presentation pane port: #port) value equals: 42.
	(presentation pane port: #port) value: 43.
	self assert: port value equals: 43
]

{ #category : #tests }
GLMPortTest >> testResetValue [
	| port |
	port := GLMTestPane new port: #port1.
	port value: 42.
	self assert: port value equals: 42.
	port resetValue.
	self assert: port value isNil
]

{ #category : #tests }
GLMPortTest >> testSimplePort [
	| port |
	port := GLMSimplePort new.
	port name: #foo.
	port value: 42.
	self assert: port name equals: #foo.
	self assert: port value equals: 42.
	self assert: port hasPane not
]

{ #category : #tests }
GLMPortTest >> testTransientValue [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	(browser transmit)
		fromOutsidePort: #entity;
		to: #one port: #entity.
	(browser transmit)
		from: #one port: #entity;
		to: #two;
		andShow: [ :a | a text ].
	browser startOn: 42.
	self assert: (browser panes first port: #entity) value equals: 42.
	self assert: (browser panes last port: #entity) value equals: 42.
	(browser panes first port: #entity) transientValue: 43.
	self assert: (browser pane port: #entity) value equals: 42.
	self assert: (browser panes first port: #entity) value equals: 42.
	self assert: (browser panes last port: #entity) value equals: 43.
	(browser pane port: #entity) transientValue: 44.
	self assert: (browser pane port: #entity) value equals: 42.
	self assert: (browser panes first port: #entity) value equals: 44.
	self assert: (browser panes last port: #entity) value equals: 44
]
