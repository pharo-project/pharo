Class {
	#name : #FLProcessBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #accessing }
FLProcessBasicSerializationTest class >> defaultTimeLimit [
	^ 1 minute
]

{ #category : #running }
FLProcessBasicSerializationTest >> setUpSerializer [
	^ serializer := FLDelayedSerializerMock newDefault
]

{ #category : #tests }
FLProcessBasicSerializationTest >> testSerializingShortDelay [
	| process |
	process := [ 2 timesRepeat: [ | d |
		d := Delay forMilliseconds: 50.
		d wait ] ] forkAt: Processor userBackgroundPriority.
	
	self 
		shouldnt: [ self serialize: process ]
		raise: FLObjectNotFound
]
