"
A test mock where a global send is not present
"
Class {
	#name : #FLGlobalSendNotPresentMock,
	#superclass : #Object,
	#category : 'Fuel-Tests-Core-Mocks'
}

{ #category : #serialization }
FLGlobalSendNotPresentMock >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitGlobalSend: self name: #FLKeyNotPresentInSmalltalks selector: #someSelector
]
