"
SUnit tests for basic in memory serialization
"
Class {
	#name : #FLInMemoryBasicSerializationTest,
	#superclass : #FLBasicSerializationTest,
	#instVars : [
		'byteArray'
	],
	#category : #'Fuel-Tests-Core-Base'
}

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
	materialized := self
		resultOfSerializeAndMaterialize:
			(Array
				with: 'a string'
				with: Smalltalk
				with: [ Smalltalk vm.
					'a string' ]).
	self assert: materialized first equals: 'a string'.
	self assert: materialized second identicalTo: Smalltalk
]
