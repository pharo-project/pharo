"
A test mock where a new instance is enforced
"
Class {
	#name : #FLSingletonMockEnforced,
	#superclass : #FLSingletonMock,
	#category : #'FuelTests-Mocks'
}

{ #category : #fuel }
FLSingletonMockEnforced class >> fuelNew [
	^ self instance
]
