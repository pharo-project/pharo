"
A SignalTest is xxxxxxxxx.

"
Class {
	#name : #SignalTest,
	#superclass : #TestCase,
	#category : #'Beacon-Core-Tests-Tests'
}

{ #category : #tests }
SignalTest >> testPrintOneLineString [
	| signal |
	signal := BeaconSignal new.
	self assert: (' ' split: signal printOneLineString) second equals: 'BeaconSignal'
 
]

{ #category : #tests }
SignalTest >> testTimestampByDefault [
	| before signal after |
	before := DateAndTime now.
	signal := BeaconSignal new.
	after := DateAndTime now.
	self assert: before <= signal timestamp.
	self assert: after >= signal timestamp.
	
]
