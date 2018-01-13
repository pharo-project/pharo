"
SUnit tests for basic in memory serialization
"
Class {
	#name : #FLInMemoryBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#instVars : [
		'byteArray'
	],
	#category : #FuelTests
}

{ #category : #testing }
FLInMemoryBasicSerializationTest class >> shouldInheritSelectors [ 
	^ true
]

{ #category : #running }
FLInMemoryBasicSerializationTest >> materialization [

	self error: 'Disabled, instead use #materialized.'

]

{ #category : #running }
FLInMemoryBasicSerializationTest >> materialized [

	^ FLMaterializer materializeFromByteArray: byteArray.

]

{ #category : #running }
FLInMemoryBasicSerializationTest >> serialize: anObject [

	byteArray := FLSerializer serializeToByteArray: anObject
]

{ #category : #tests }
FLInMemoryBasicSerializationTest >> testConsiderCustomGlobal [
	"Since FLSerializer class >> serializeToByteArray: does not allow customizing serialization, this test is disabled."
]

{ #category : #tests }
FLInMemoryBasicSerializationTest >> testConsiderCustomWideSymbolGlobal [
	"Since FLSerializer class >> serializeToByteArray: does not allow customizing serialization, this test is disabled."
	self error "Since it's an expectedFailure"
]

{ #category : #tests }
FLInMemoryBasicSerializationTest >> testStringGlobalsAndClosure [

	| materialized |
	materialized := self resultOfSerializeAndMaterialize:  (Array 
			with: 'a string' 
			with: Transcript
			with: [ Transcript show: 'a string' ]).
	self assert: materialized first = 'a string'.
	self assert: materialized second == Transcript
]
