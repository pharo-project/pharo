"
A test mock which is not serializable
"
Class {
	#name : #FLNotSerializableMock,
	#superclass : #Object,
	#category : #'Fuel-Tests-Core-Support-Mocks'
}

{ #category : #hooks }
FLNotSerializableMock >> fuelAccept: aGeneralMapper [

	aGeneralMapper visitNotSerializable: self
]
