Class {
	#name : 'FLProcessSerializationTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
FLProcessSerializationTest >> setUpSerializer [
	super setUpSerializer.
	
	self serializer
		at: #useDelayedMock
		put: true
]

{ #category : 'tests' }
FLProcessSerializationTest >> testSerializingShortDelay [
	| process |
	process := [ 2 timesRepeat: [ | d |
		d := Delay forMilliseconds: 50.
		d wait ] ] forkAt: Processor userBackgroundPriority.
	
	self 
		shouldnt: [ self serialize: process ]
		raise: FLObjectNotFound.
		
	self assert: (self serializer at: #delayedMockUsed)
]
