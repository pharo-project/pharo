"
Superclass for serialization tests
"
Class {
	#name : #FLSerializationTest,
	#superclass : #TestCase,
	#instVars : [
		'serializer',
		'materializer',
		'classFactory',
		'streamFactory',
		'traits'
	],
	#category : #FuelTests
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

{ #category : #'traits-factory' }
FLSerializationTest >> cleanUpTraits [
	self traits do: [ :aTrait | self removeFromSystem: aTrait ]
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
FLSerializationTest >> newTraitNamed: traitName inCategory: aCategory [

	^ Trait 
		named: traitName asSymbol
		uses: Array new 
		category: aCategory asSymbol
]

{ #category : #'traits-factory' }
FLSerializationTest >> newTraitSuffixed: suffix [

	| trait |
	trait := Trait 
		named: (self traitNamePrefix, suffix) asSymbol
		uses: Array new 
		category: (self classFactory packageName, '-', self classFactory defaultCategoryPostfix) asSymbol.
	self traits add: trait.
	
	^ trait
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
	self useMemoryStream.

]

{ #category : #running }
FLSerializationTest >> setUpClassFactory [
	classFactory := ClassFactoryForTestCase new

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
	
	"Traits should be cleaned before classFactory because it seems class factory only knwos how to clean classes, not traits."
	self tearDownTraits.
	self tearDownClassFactory.
	self tearDownGlobalVariables.
	self streamFactory tearDown.
	self tearDownInstanceVariables.
	super tearDown.
]

{ #category : #running }
FLSerializationTest >> tearDownClassFactory [
	self classFactory cleanUp
]

{ #category : #running }
FLSerializationTest >> tearDownGlobalVariables [

	Smalltalk globals removeKey: #FLGlobalVariableForTesting ifAbsent: []
]

{ #category : #running }
FLSerializationTest >> tearDownInstanceVariables [
	self class withAllSuperclasses do: [ :class |
		class = TestCase ifTrue: [ ^ self ].
		
		class instVarNames do: [ :varName |
			self 
				instVarNamed: varName
				put: nil ] ]
]

{ #category : #running }
FLSerializationTest >> tearDownTraits [
	self cleanUpTraits
]

{ #category : #'traits-factory' }
FLSerializationTest >> traitNamePrefix [
	^'FLTraitForTesting'
]

{ #category : #accessing }
FLSerializationTest >> traits [	
	^ traits ifNil: [ traits := OrderedCollection new ]
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useBinaryFileStream [

	streamFactory := FLFileStreamStrategy newWithBinaryFileStream
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useGzipInMemoryStream [

	streamFactory := FLGZipStrategy newWithTarget: FLByteArrayStreamStrategy new
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useInMemorySerializationStream [ 
	"This is a special factory to test what we offer with FLSerializer class >> #serializeInMemory:  and  FLMaterializer class >> #materializeFromByteArray: "
	streamFactory := FLByteArrayStreamStrategy new
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useMemoryStream [

	streamFactory := FLMultiByteStreamStrategy new
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useMultiByteFileStream [

	streamFactory := FLFileStreamStrategy newWithMultiByteFileStream
]

{ #category : #'stream-strategies' }
FLSerializationTest >> useStandardFileStream [

	streamFactory := FLFileStreamStrategy newWithStandardFileStream
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
