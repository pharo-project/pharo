"
SUnit tests for fuel serialization of global sends
"
Class {
	#name : #FLGlobalSendSerializationTest,
	#superclass : #FLSerializationTest,
	#category : 'FuelTests'
}

{ #category : #tests }
FLGlobalSendSerializationTest >> testBasic [

	| result |
	self deny: FLGlobalSendMock newInstanceToSerialize = FLGlobalSendMock newInstanceToMaterialize.

	result := self resultOfSerializeAndMaterialize: FLGlobalSendMock newInstanceToSerialize.
	self assert: FLGlobalSendMock newInstanceToMaterialize = result.
]

{ #category : #tests }
FLGlobalSendSerializationTest >> testGlobalNotFound [

	self should: [ self resultOfSerializeAndMaterialize: FLGlobalSendNotPresentMock new ]
		raise: FLGlobalNotFound
		whoseDescriptionIncludes: #FLKeyNotPresentInSmalltalks 
		description: 'Should raise an error when global name is not present in Smalltalks globals.' 
	

]
