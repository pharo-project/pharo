"
A test mock where a global send is not present
"
Class {
	#name : #FLGlobalSendNotPresentMock,
	#superclass : #Object,
	#category : #FuelTests-Mocks
}

{ #category : #serialization }
FLGlobalSendNotPresentMock >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitGlobalSend: self name: #FLKeyNotPresentInSmalltalks selector: #someSelector
]
