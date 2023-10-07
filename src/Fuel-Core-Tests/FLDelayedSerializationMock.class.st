"
I am a mock used to simlify testing.
	
A mock is an object that simulates the behavior of a real object or component in a controlled manner. It is used in unit testing to isolate the code being tested from its dependencies, allowing the code to be tested in isolation.
"
Class {
	#name : 'FLDelayedSerializationMock',
	#superclass : 'FLSerialization',
	#instVars : [
		'duration'
	],
	#category : 'Fuel-Core-Tests-Support-Mocks',
	#package : 'Fuel-Core-Tests',
	#tag : 'Support-Mocks'
}

{ #category : 'testing' }
FLDelayedSerializationMock class >> isResponsible [
	^ self configuration class = FLSerializerMock and: [
		self configuration
			at: #useDelayedMock
			ifAbsent: [ false ] ]
]

{ #category : 'accessing' }
FLDelayedSerializationMock >> duration [
	^ duration ifNil: [
		100 milliSeconds ]
]

{ #category : 'accessing' }
FLDelayedSerializationMock >> duration: aDuration [
	duration := aDuration
]

{ #category : 'private' }
FLDelayedSerializationMock >> instancesStep [
	super instancesStep.
	
	self wait
]

{ #category : 'running' }
FLDelayedSerializationMock >> run [
	self configuration
		at: #delayedMockUsed
		put: true.
		
	super run
]

{ #category : 'private' }
FLDelayedSerializationMock >> wait [
	self duration asDelay wait
]
