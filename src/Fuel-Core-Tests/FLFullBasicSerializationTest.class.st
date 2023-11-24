Class {
	#name : 'FLFullBasicSerializationTest',
	#superclass : 'FLBasicSerializationTest',
	#classVars : [
		'ClassVariableForTesting'
	],
	#category : 'Fuel-Core-Tests-FullSerialization',
	#package : 'Fuel-Core-Tests',
	#tag : 'FullSerialization'
}

{ #category : 'accessing' }
FLFullBasicSerializationTest class >> packageNamesUnderTest [
	^ #('FuelMetalevel')
]

{ #category : 'testing' }
FLFullBasicSerializationTest class >> shouldInheritSelectors [ 
	^true
]

{ #category : 'accessing' }
FLFullBasicSerializationTest >> defaultTimeLimit [
	^ 2 minutes
]

{ #category : 'initialization' }
FLFullBasicSerializationTest >> initialize [
		<ignoreUnusedClassVariables: #( ClassVariableForTesting )>
		super initialize
]

{ #category : 'tests' }
FLFullBasicSerializationTest >> testClassGlobalAssociation [
	"An association included in Smalltalk globals is treated as global by default."

	self assertSerializationIdentityOf: (self environmentOfTest associationAt: #FLPerson)
]

{ #category : 'tests' }
FLFullBasicSerializationTest >> testClassSideCompiledMethod [
	"They are not considered globals by default in full serialization mode."
	self flag: #todo. "This is really undesired. They should be global by default, as in light serialization mode."
	self resultOfSerializeAndMaterializeCompiledMethod: FLGlobalSendMock class >> #newInstanceToSerialize.
]

{ #category : 'tests' }
FLFullBasicSerializationTest >> testClassVariable [
	"An association included in a global class is treated as global by default."

	self assertSerializationIdentityOf: (self class classVarNamed: #ClassVariableForTesting)
]

{ #category : 'tests' }
FLFullBasicSerializationTest >> testCompiledMethod [
	"They are not considered globals by default in full serialization mode."
	self flag: #todo. "This is really undesired. They should be global by default, as in light serialization mode."
	self resultOfSerializeAndMaterialize: FLPair >> #left.
]

{ #category : 'running' }
FLFullBasicSerializationTest >> testConsiderGlobal: anObject [
	"With explicit specification of a symbol as global, both value and association keep identity."

	| globalAssociation |
	self serializer lookUpInGlobals: #FLGlobalVariableForTesting.

	self environmentOfTest
		at: #FLGlobalVariableForTesting
		put: anObject.
	addedGlobals add: #FLGlobalVariableForTesting.
	globalAssociation := self environmentOfTest associationAt: #FLGlobalVariableForTesting.
	
	self assertSerializationIdentityOf: globalAssociation.
	self assertSerializationIdentityOf: globalAssociation value.
]

{ #category : 'tests-considerGlobal' }
FLFullBasicSerializationTest >> testConsiderGlobalAClass [
	"With explicit specification of a symbol as global, both value and association keep identity."
	
	self testConsiderGlobal: FLPerson.
]

{ #category : 'tests-considerGlobal' }
FLFullBasicSerializationTest >> testConsiderGlobalAnObject [
	"With explicit specification of a symbol as global, both value and association keep identity."
	
	self testConsiderGlobal: FLPerson new.
]

{ #category : 'tests-bugs' }
FLFullBasicSerializationTest >> testCreateClassWithChangedSuperclassFormat [
	"Tests issue #221"

	| a b |
	a := self classFactory make: [ :aBuilder | aBuilder slotsFromString: 'one' ].
	b := self classFactory make: [ :aBuilder |
		     aBuilder
			     superclass: a;
			     slotsFromString: 'two' ].
	self serializer fullySerializeBehavior: b.
	self serialize: {
			b new.
			b }.

	a addInstVarNamed: 'newIVar'.

	self shouldnt: [ self materialized ] raise: PrimitiveFailed
]

{ #category : 'tests-long' }
FLFullBasicSerializationTest >> testMethodDictionaries [
	"Tests correct serialization of all the method dictionaries in the System package."

	((self packageOrganizer packages select: [ :package | package name beginsWith: 'System' ]) flatCollect: [ :package | package definedClasses ]) do: [ :class |
		self resultOfSerializeAndMaterializeMethodDictionary: class methodDictionary ]
]

{ #category : 'tests' }
FLFullBasicSerializationTest >> testSmalltalkGlobalAssociation [
	"An association included in Smalltalk globals is treated as global by default."

	self assertSerializationIdentityOf: (self environmentOfTest associationAt: #Smalltalk)
]
