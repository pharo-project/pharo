"
Superclass for serialization tests
"
Class {
	#name : #FLSerializationTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		#streamStrategy,
		#classFactory,
		#materializer,
		#serializer
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #accessing }
FLSerializationTest class >> packageNamesUnderTest [
	^ #('Fuel')
]

{ #category : #accessing }
FLSerializationTest class >> resources [
	^ { FLFilePoolTestResource }
]

{ #category : #helpers }
FLSerializationTest >> assertSerializationEqualityOf: anObject [
	"Asserts that the original object and the materialized one are equal (but not the same)"

	| materialized |
	materialized := self resultOfSerializeAndMaterialize: anObject.
	self assert: anObject ~~ materialized description: 'The materialized object should not be the same as the serialized one'.
	self assert: anObject = materialized description: 'The materialized object should be equal to serialized one'.
]

{ #category : #helpers }
FLSerializationTest >> assertSerializationEqualityOrIdentityOf: anObject [
	"Asserts that the original object and the materialized one are equal (may be the same)"

	| materialized |
	materialized := self resultOfSerializeAndMaterialize: anObject.
	self assert: anObject = materialized description: 'The materialized object should be equal to serialized one'.
]

{ #category : #helpers }
FLSerializationTest >> assertSerializationIdentityOf: anObject [
	"Asserts that the original object and the materialized one are the same"

	| result |
	result := self resultOfSerializeAndMaterialize: anObject.
	self 
		assert: anObject == result 
		description: 'The materialized object is not the same as the serialized one'.
]

{ #category : #accessing }
FLSerializationTest >> classFactory [
	^classFactory
]

{ #category : #cleaning }
FLSerializationTest >> deleteFilePath: aFilename [
	 FLPlatform current deleteFilePath: aFilename
]

{ #category : #testing }
FLSerializationTest >> expectedFailures [

	^ FLPlatform current expectedFailuresOf: self class
]

{ #category : #helpers }
FLSerializationTest >> materialized [
	^ self materializedObjects root
]

{ #category : #helpers }
FLSerializationTest >> materializedObjects [
	^ self materializer materialize
]

{ #category : #accessing }
FLSerializationTest >> materializer [
	^ materializer
]

{ #category : #'class-factory' }
FLSerializationTest >> newClassOrTrait [
	^ self classFactory silentlyNewClass
]

{ #category : #helpers }
FLSerializationTest >> resultOfSerializeAndMaterialize: anObject [
	^ self
		serialize: anObject;
		materialized
]

{ #category : #helpers }
FLSerializationTest >> resultOfSerializeAndMaterializeCompiledMethod: aCompiledMethod [
	| materialized |
	materialized := self resultOfSerializeAndMaterialize: aCompiledMethod.
	self assert: (materialized isEqualRegardlessTrailerTo: aCompiledMethod)
]

{ #category : #helpers }
FLSerializationTest >> resultOfSerializeAndMaterializeMethod: aCompiledMethod [
	self serializer fullySerializeMethod: aCompiledMethod. 
	self serialize: aCompiledMethod.
	^ self materialized
]

{ #category : #helpers }
FLSerializationTest >> resultOfSerializeAndMaterializeMethodDictionary: aMethodDictionary [
	| materialized |
	materialized := self resultOfSerializeAndMaterialize: aMethodDictionary.
	self assert: (materialized isEqualRegardlessMethodsTrailerTo: aMethodDictionary)
]

{ #category : #helpers }
FLSerializationTest >> serializationOf: anObject includes: anotherObject [ 
	self serialize: anObject.
	^ self materializedObjects objects includes: anotherObject
]

{ #category : #helpers }
FLSerializationTest >> serialize: anObject [
	self serializer
		object: anObject;
		serialize
]

{ #category : #accessing }
FLSerializationTest >> serializer [
	^ serializer
]

{ #category : #running }
FLSerializationTest >> setUp [
	super setUp.
	
	self
		setUpEnvironment;
		setUpClassFactory;
		setUpStreamStrategy;
		setUpSerializer;
		setUpMaterializer
]

{ #category : #running }
FLSerializationTest >> setUpClassFactory [
	classFactory := FLPlatform current classFactoryForTestCaseClass
		forEnvironment: self environmentOfTest
]

{ #category : #running }
FLSerializationTest >> setUpEnvironment [
	self testingEnvironment: (FLEnvironmentProtectingWrapper on: Smalltalk globals)
]

{ #category : #running }
FLSerializationTest >> setUpMaterializer [
	materializer := FLMaterializerMock new
		environment: self environmentOfTest;
		streamFactory: [ streamStrategy readStream ]
		yourself
]

{ #category : #running }
FLSerializationTest >> setUpSerializer [
	serializer := FLSerializerMock new
		environment: self environmentOfTest;
		streamFactory: [ streamStrategy writeStream ]
		yourself
]

{ #category : #running }
FLSerializationTest >> setUpStreamStrategy [
	streamStrategy := FLFileReferenceStreamStrategy new
]

{ #category : #running }
FLSerializationTest >> tearDown [
	self tearDownClassFactory.
	
	super tearDown
]

{ #category : #running }
FLSerializationTest >> tearDownClassFactory [
	self classFactory cleanUp
]
