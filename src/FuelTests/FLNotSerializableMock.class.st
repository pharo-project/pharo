"
A test mock which is not serializable
"
Class {
	#name : #FLNotSerializableMock,
	#superclass : #Object,
	#category : #FuelTests-Mocks
}

{ #category : #hooks }
FLNotSerializableMock >> fuelAccept: aGeneralMapper [

	aGeneralMapper visitNotSerializable: self
]
