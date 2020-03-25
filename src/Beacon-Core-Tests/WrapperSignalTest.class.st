"
A WrapperSignalTest is xxxxxxxxx.

"
Class {
	#name : #WrapperSignalTest,
	#superclass : #TestCase,
	#category : #'Beacon-Core-Tests-Tests'
}

{ #category : #tests }
WrapperSignalTest >> testAsBeaconSignalDoesNotAffectAnExistingSignal [
	| signal |
	signal := StringSignal new.
	self assert: signal asBeaconSignal equals: signal
]

{ #category : #tests }
WrapperSignalTest >> testAsBeaconSignalTransformsIntoWrapperSignal [
	self assert: (42 asBeaconSignal isKindOf: WrapperSignal).
	self assert: 42 asBeaconSignal target equals: 42.

]
