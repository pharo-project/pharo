"
Superclass for serialization tests
"
Class {
	#name : #FLSerializationTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'serializer',
		'materializer',
		'classFactory',
		'streamFactory'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #accessing }
FLSerializationTest class >> packageNamesUnderTest [
	^ #('Fuel')
]

{ #category : #accessing }
FLSerializationTest >> analyzer [
	^serializer analyzer
]

{ #category : #running }
FLSerializationTest >> assertSerializationEqualityOf: anObject [
	"Asserts that the original object and the materialized one are equal (but not the same)"

	| materialized |
	materialized := self resultOfSerializeAndMaterialize: anObject.
	self assert: anObject ~~ materialized description: 'The materialized object should not be the same as the serialized one'.
	self assert: anObject = materialized description: 'The materialized object should be equal to serialized one'.
]

{ #category : #running }
FLSerializationTest >> assertSerializationEqualityOrIdentityOf: anObject [
	"Asserts that the original object and the materialized one are equal (may be the same)"

	| materialized |
	materialized := self resultOfSerializeAndMaterialize: anObject.
	self assert: anObject = materialized description: 'The materialized object should be equal to serialized one'.
]

{ #category : #running }
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
FLSerializationTest >> deleteFileNamed: aFilename [
	 FLPlatform current deleteFileNamed: aFilename
]

{ #category : #running }
FLSerializationTest >> materialization [
	
	self streamFactory readStreamDo: [:aStream |
		^ self materializer materializeFrom: aStream ]
]

{ #category : #running }
FLSerializationTest >> materializationHeader [
	
	self streamFactory readStreamDo: [:aStream |
		^ self materializer materializeHeaderFrom: aStream ]
]

{ #category : #running }
FLSerializationTest >> materialized [
	
	^ self materialization root
]

{ #category : #running }
FLSerializationTest >> materializedObjects [
	
	^ self materialization objects
]

{ #category : #accessing }
FLSerializationTest >> materializer [
	^materializer
]

{ #category : #'class-factory' }
FLSerializationTest >> newClass [
	^ self newSubclassOf: Object 
		instanceVariableNames: '' 
		classVariableNames: ''

]

{ #category : #'class-factory' }
FLSerializationTest >> newClassInCategory: aCategory [
	^ self classFactory silentlyNewClassInCategory: aCategory
]

{ #category : #'class-factory' }
FLSerializationTest >> newSubclassNamed: nameString of: aClass  instanceVariableNames: ivNamesString classVariableNames: classVarsString [
	| class |
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		class := aClass
			subclass: nameString asSymbol
			instanceVariableNames: ivNamesString
			classVariableNames:  classVarsString
			category: self classFactory defaultCategory
	].
	self classFactory createdClasses add: class.
	^ class
]

{ #category : #'class-factory' }
FLSerializationTest >> newSubclassOf: aClass  instanceVariableNames: ivNamesString classVariableNames: classVarsString [
	^ self classFactory silentlyNewSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString
]

{ #category : #'traits-factory' }
FLSerializationTest >> newTraitSuffixed: suffix [
	^ self classFactory
		newTraitNamed: (self traitNamePrefix, suffix) asSymbol
		uses: Array new 
		tag: (self classFactory packageName, '-', self classFactory defaultTagPostfix) asSymbol
]

{ #category : #'traits-factory' }
FLSerializationTest >> registerBehavior: aBehavior [
	aBehavior isTrait
		ifTrue: [ self classFactory createdTraits add: aBehavior ]
		ifFalse: [ self classFactory createdClasses add: aBehavior ]
]

{ #category : #cleaning }
FLSerializationTest >> removeFromSystem: aClassOrTrait [
	aClassOrTrait removeFromSystem: false
]

{ #category : #running }
FLSerializationTest >> resultOfSerializeAndMaterialize: anObject [

	self serialize: anObject.
	^ self materialized

]

{ #category : #running }
FLSerializationTest >> resultOfSerializeAndMaterializeCompiledMethod: aCompiledMethod [
	| materialized |
	materialized := self resultOfSerializeAndMaterialize: aCompiledMethod.
	self assert: (materialized isEqualRegardlessTrailerTo: aCompiledMethod)
]

{ #category : #running }
FLSerializationTest >> resultOfSerializeAndMaterializeMethod: aCompiledMethod [
	self analyzer considerInternalMethod: aCompiledMethod. 
	self serialize: aCompiledMethod.
	^ self materialized

]

{ #category : #running }
FLSerializationTest >> resultOfSerializeAndMaterializeMethodDictionary: aMethodDictionary [
	| materialized |
	materialized := self resultOfSerializeAndMaterialize: aMethodDictionary.
	self assert: (materialized isEqualRegardlessMethodsTrailerTo: aMethodDictionary)
]

{ #category : #running }
FLSerializationTest >> serializationOf: anObject [ 
	self streamFactory writeStreamDo: [:aStream |
	^ serializer serialize: anObject on: aStream. ].

]

{ #category : #running }
FLSerializationTest >> serializationOf: anObject includes: anotherObject [ 
	| serialization |
	self streamFactory writeStreamDo: [:aStream |
	serialization := serializer serialize: anObject on: aStream.
	^ serialization objects includes: anotherObject ].
]

{ #category : #running }
FLSerializationTest >> serialize: anObject [

	self streamFactory writeStreamDo: [:aStream |
		self serializer serialize: anObject on: aStream ]
]

{ #category : #accessing }
FLSerializationTest >> serializer [
	^serializer
]

{ #category : #running }
FLSerializationTest >> setUp [
	super setUp.
	self setUpClassFactory.
	self setUpSerializer.
	self setUpMaterializer.
	"You have several stream strategies available on 'stream-strategies' category."
	self useFileReferenceStream.

	"When rerunning tests from the test runner #initialize
	will not be sent to the instance but the previous run will
	have torn down the variable"
	testingEnvironment ifNil: [ 
		testingEnvironment := Smalltalk globals ]
]

{ #category : #running }
FLSerializationTest >> setUpClassFactory [
	classFactory := FLPlatform current classFactoryForTestCaseClass new

]

{ #category : #running }
FLSerializationTest >> setUpMaterializer [
	materializer := FLMaterializer newDefault
]

{ #category : #running }
FLSerializationTest >> setUpSerializer [
	serializer := FLSerializer newDefault
]

{ #category : #accessing }
FLSerializationTest >> streamFactory [
	^streamFactory
]

{ #category : #running }
FLSerializationTest >> tearDown [
	self
		tearDownClassFactory;
		tearDownGlobalVariables.
	self streamFactory tearDown.
	
	super tearDown
]

{ #category : #running }
FLSerializationTest >> tearDownClassFactory [
	self classFactory cleanUp
]

{ #category : #running }
FLSerializationTest >> tearDownGlobalVariables [
	testingEnvironment
		removeKey: #FLGlobalVariableForTesting
		ifAbsent: []
]

{ #category : #'traits-factory' }
FLSerializationTest >> traitNamePrefix [
	^'FLTraitForTesting'
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useBinaryFileStream [

	streamFactory := FLBinaryFileStreamStrategy new
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useByteArrayStream [ 
	"This is a special factory to test what we offer with FLSerializer class >> #serializeInMemory:  and  FLMaterializer class >> #materializeFromByteArray: "
	streamFactory := FLByteArrayStreamStrategy new
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useFileReferenceStream [

	streamFactory := FLFileReferenceStreamStrategy new
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useGzipInMemoryStream [

	streamFactory := FLGZipStrategy newWithTarget: FLByteArrayStreamStrategy new
]

{ #category : #'class-factory' }
FLSerializationTest >> withNotificationsNewClass [
	^ self classFactory withNotificationsNewClass
]

{ #category : #'class-factory' }
FLSerializationTest >> withNotificationsNewClassWithInstanceVariableNames: instanceVariableNames [
	"Returns a class for testing, with the specified instance variables."

	^ classFactory withNotificationsNewClassWithInstanceVariableNames: instanceVariableNames
]
