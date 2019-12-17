"
SUnit tests for global fuel environment
"
Class {
	#name : #FLGlobalEnvironmentTest,
	#superclass : #FLSerializationTest,
	#instVars : [
		'serializationEnvironment',
		'materializationEnvironment'
	],
	#category : #'Fuel-Tests-Core-Base'
}

{ #category : #running }
FLGlobalEnvironmentTest >> setUp [ 
	super setUp.
	serializationEnvironment := Dictionary new.
	materializationEnvironment := Dictionary new.
	
	self analyzer globalEnvironment: serializationEnvironment.
	self materializer globalEnvironment: materializationEnvironment.
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testClassSideCompiledMethod [
	"Any class-side compiled method should be serialized as global by default. On materialization, it should be found in the global environment, and the bytecodes hash must be the same."

	| classA classB |
	classA := self newClass.
	classA class duringTestCompileSilently: 'm  ^ 42'.
	classB := self newClass.
	classB class duringTestCompileSilently: 'm    ^    42   '.
	self assert: (classA class >> #m) bytecodesHash equals: (classB class >> #m) bytecodesHash.
	self deny: classA class >> #m identicalTo: classB class >> #m.

	materializationEnvironment at: classA name put: classB.

	self assert: (self resultOfSerializeAndMaterialize: classA class >> #m) identicalTo: classB class >> #m
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testCompiledMethod [
	"Any compiled method should be serialized as global by default. On materialization, it should be found in the global environment, and the bytecodes hash must be the same."

	| classA classB |
	classA := self newClass
		duringTestCompileSilently: 'm  ^ 42';
		yourself.
	classB := self newClass
		duringTestCompileSilently: 'm    ^    42   ';
		yourself.
	self assert: (classA >> #m) bytecodesHash equals: (classB >> #m) bytecodesHash.
	self deny: classA >> #m identicalTo: classB >> #m.

	materializationEnvironment at: classA name put: classB.

	self assert: (self resultOfSerializeAndMaterialize: classA >> #m) identicalTo: classB >> #m
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testCompiledMethodChanged [
	"A compiled methods should be serialized as global by default. On materialization, it must be found in the global environment, and the bytecodes hash must be the same. Else, raise a proper error."
	
	| classA classB |
	classA := self newClass
		duringTestCompileSilently: 'm  ^ 42';
		yourself.
	classB := self newClass
		duringTestCompileSilently: 'm  ^ [ 42 ]';
		yourself.

	materializationEnvironment 
		at: classA name
		put: classB.

	self serialize: classA >> #m.

	self should: [ self materialization ]
		raise: FLMethodChanged 
		description: 'Serialized and materialized methods should have the same bytecodes.'
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testConsiderCustomGlobal [
	"A custom global variable is treated as global if we explicitly specify that on serialization."
	
	self analyzer considerGlobal: #FLGlobalVariableForTesting.

	serializationEnvironment 
		at: #FLGlobalVariableForTesting put: 42.

	materializationEnvironment 
		at: #FLGlobalVariableForTesting put: 7.
	
	self assert: (self resultOfSerializeAndMaterialize: 42) equals: 7
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testConsiderCustomGlobalNotFound [
	"A custom global variable is treated as global if we explicitly specify that on serialization. Then, if the variable is not defined in the materialization environment, a error should be raised."
	
	self analyzer considerGlobal: #FLGlobalVariableForTesting.

	serializationEnvironment 
		at: #FLGlobalVariableForTesting put: 42.

	self serialize: 42.
	self 
		should: [ self materialization ] 
		raise: FLGlobalNotFound 
		description: 'Raise an error since the variable is not present in materialization global environment'
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testDontConsiderCustomGlobal [
	"A custom global variable is not treated as global unless we explicitly specify during serialization."

	| anObject |
	anObject := Object new.
	serializationEnvironment at: #FLGlobalVariableForTesting put: anObject.
	materializationEnvironment
		at: #FLGlobalVariableForTesting put: anObject;
		at: #Object put: Object.

	self deny: (self analyzer globalSymbols includes: #FLGlobalVariableForTesting).
	self deny: (self resultOfSerializeAndMaterialize: anObject) identicalTo: anObject
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testGlobalClass [
	"A class should be global by default. On materialization, the global symbol should be found in the global environment."

	materializationEnvironment at: #Integer put: FLPerson.

	self assert: (self resultOfSerializeAndMaterialize: Integer) identicalTo: FLPerson
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testGlobalClassNotFound [
	"If a class is not defined in the materialization environment, a proper error should be raised."

	self serialize: Integer.
	
	self should: [ self materialization ] 
		raise: FLClassNotFound 
		description: 'Raise an error since the class is not present in materialization global environment'
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testGlobalMetaclass [
	"A metaclass should be global by default. On materialization, the global symbol should be found in the global environment."

	materializationEnvironment at: #Integer put: FLPerson.

	self assert: (self resultOfSerializeAndMaterialize: Integer class) identicalTo: FLPerson class
]

{ #category : #tests }
FLGlobalEnvironmentTest >> testGlobalMetaclassNotFound [
	"If a class is not defined in the materialization environment, a proper error should be raised."
	
	self serialize: Integer class.
	
	self should: [ self materialization ] 
		raise: FLClassNotFound 
		description: 'Raise an error since the class is not present in materialization global environment'
]
