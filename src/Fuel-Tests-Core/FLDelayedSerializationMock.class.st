Class {
	#name : #FLDelayedSerializationMock,
	#superclass : #FLSerialization,
	#instVars : [
		#duration
	],
	#category : #'Fuel-Tests-Core-Support-Mocks'
}

{ #category : #testing }
FLDelayedSerializationMock class >> isResponsible [
	^ self configuration class = FLSerializerMock and: [
		self configuration
			at: #useDelayedMock
			ifAbsent: [ false ] ]
]

{ #category : #accessing }
FLDelayedSerializationMock >> duration [
	^ duration ifNil: [
		100 milliSeconds ]
]

{ #category : #accessing }
FLDelayedSerializationMock >> duration: aDuration [
	duration := aDuration
]

{ #category : #private }
FLDelayedSerializationMock >> instancesStep [
	super instancesStep.
	
	self wait
]

{ #category : #running }
FLDelayedSerializationMock >> run [
	self configuration
		at: #delayedMockUsed
		put: true.
		
	super run
]

{ #category : #private }
FLDelayedSerializationMock >> wait [
	self duration asDelay wait
]
