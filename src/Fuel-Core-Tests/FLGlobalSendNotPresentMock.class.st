"
A test mock where a global send is not present
"
Class {
	#name : 'FLGlobalSendNotPresentMock',
	#superclass : 'Object',
	#category : 'Fuel-Core-Tests-Support-Mocks',
	#package : 'Fuel-Core-Tests',
	#tag : 'Support-Mocks'
}

{ #category : 'serialization' }
FLGlobalSendNotPresentMock >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitGlobalSend: self name: #FLKeyNotPresentInSmalltalks selector: #someSelector
]
