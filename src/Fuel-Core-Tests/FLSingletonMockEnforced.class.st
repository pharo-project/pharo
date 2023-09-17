"
A test mock where a new instance is enforced
"
Class {
	#name : 'FLSingletonMockEnforced',
	#superclass : 'FLSingletonMock',
	#category : 'Fuel-Core-Tests-Support-Mocks',
	#package : 'Fuel-Core-Tests',
	#tag : 'Support-Mocks'
}

{ #category : 'fuel' }
FLSingletonMockEnforced class >> fuelNew [
	^ self instance
]
