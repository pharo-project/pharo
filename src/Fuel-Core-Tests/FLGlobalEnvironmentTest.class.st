"
SUnit tests for global fuel environment
"
Class {
	#name : 'FLGlobalEnvironmentTest',
	#superclass : 'FLSerializationTest',
	#category : 'Fuel-Core-Tests-Base',
	#package : 'Fuel-Core-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testClassSideCompiledMethod [
	"Any class-side compiled method should be serialized as global by default. On materialization, it should be found in the global environment, and the bytecodes hash must be the same."

	| classA classB |
	classA := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm  ^ 42'
		in: classA class.
	classB := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm   ^   42  '
		in: classB class.
	self assert: (classA class >> #m) bytecodesHash equals: (classB class >> #m) bytecodesHash.
	self deny: classA class >> #m identicalTo: classB class >> #m.

	self materializer environment: (Dictionary new
		at: classA name put: classB;
		yourself).

	self assert: (self resultOfSerializeAndMaterialize: classA class >> #m) identicalTo: classB class >> #m
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testCompiledMethod [
	"Any compiled method should be serialized as global by default. On materialization, it should be found in the global environment, and the bytecodes hash must be the same."

	| classA classB |
	classA := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm ^ 42'
		in: classA.
	classB := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm    ^   42  '
		in: classB.
	self assert: (classA >> #m) bytecodesHash equals: (classB >> #m) bytecodesHash.
	self deny: classA >> #m identicalTo: classB >> #m.

	self materializer environment: (Dictionary new
		at: classA name put: classB;
		yourself).

	self assert: (self resultOfSerializeAndMaterialize: classA >> #m) identicalTo: classB >> #m
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testCompiledMethodChanged [
	"A compiled methods should be serialized as global by default.
	On materialization, it must be found in the global environment, and the bytecodes hash must be the same.
	Else, raise a proper error."
	
	| classA classB |
	Smalltalk compiler compilationContext optionCleanBlockClosure ifTrue: [ ^ self skip ].

	classA := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile:  'm  ^ 42'
		in: classA.
	classB := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm  ^ [ 42 yourself ]'
		in: classB.

	self materializer environment: (Dictionary new
		at: classA name put: classB;
		yourself).

	self serialize: classA >> #m.

	self
		should: [ self materialized ]
		raise: FLMethodChanged 
		description: 'Serialized and materialized methods should have the same bytecodes.'
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testCompiledMethodChangedConstant [
	"A compiled methods should be serialized as global by default.
	On materialization, it must be found in the global environment, and the bytecodes hash must be the same.
	Changes to closures are not relevant."
	
	| classA classB |
	classA := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile:  'm  ^ [ 41 ]'
		in: classA.
	classB := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm  ^ [ 42 ]'
		in: classB.

	self materializer environment: (Dictionary new
		at: classA name put: classB;
		yourself).

	self serialize: classA >> #m.

	self
		shouldnt: [ self materialized ]
		raise: FLMethodChanged
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testCompiledMethodChangedNonClean [
	"A compiled methods should be serialized as global by default.
	On materialization, it must be found in the global environment, and the bytecodes hash must be the same.
	Changes to closures are not relevant."
	
	| classA classB |
	classA := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile:  'm  ^ [ 42 yourself ]'
		in: classA.
	classB := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm  ^ [ 42 + 3 ]'
		in: classB.

	self materializer environment: (Dictionary new
		at: classA name put: classB;
		yourself).

	self serialize: classA >> #m.

	self
		shouldnt: [ self materialized ]
		raise: FLMethodChanged
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testCompiledMethodNotChangedConstant [
	"A compiled methods should be serialized as global by default.
	On materialization, it must be found in the global environment, and the bytecodes hash must be the same.
	A method with a constant closure has the same byte codes as a method with just the literal."
	
	| classA classB |
	classA := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile:  'm  ^ [ 41 ]'
		in: classA.
	classB := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'm  ^ 41'
		in: classB.

	self materializer environment: (Dictionary new
		at: classA name put: classB;
		yourself).

	self serialize: classA >> #m.

	self
		shouldnt: [ self materialized ]
		raise: FLMethodChanged
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testConsiderCustomGlobal [
	"A custom global variable is treated as global if we explicitly specify that on serialization."
	
	self serializer
		lookUpInGlobals: #FLGlobalVariableForTesting;
		environment: (Dictionary new
			at: #FLGlobalVariableForTesting put: 42;
			yourself).
	addedGlobals add: #FLGlobalVariableForTesting. 

	self materializer environment: (Dictionary new
		at: #FLGlobalVariableForTesting put: 7;
		yourself).
	
	self assert: (self resultOfSerializeAndMaterialize: 42) equals: 7
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testConsiderCustomGlobalNotFound [
	"A custom global variable is treated as global if we explicitly specify that on serialization. Then, if the variable is not defined in the materialization environment, a error should be raised."
	
	self serializer
		lookUpInGlobals: #FLGlobalVariableForTesting;
		environment: (Dictionary new
			at: #FLGlobalVariableForTesting put: 42;
			yourself).
	addedGlobals add: #FLGlobalVariableForTesting.
	self materializer environment: Dictionary new.

	self serialize: 42.
	self 
		should: [ self materialized ] 
		raise: FLGlobalNotFound 
		description: 'Raise an error since the variable is not present in materialization global environment'
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testDontConsiderCustomGlobal [
	"A custom global variable is not treated as global unless we explicitly specify during serialization."

	| anObject |
	anObject := Object new.
	self serializer
		environment: (Dictionary new
			at: #FLGlobalVariableForTesting put: anObject;
			at: #Object put: Object;
			yourself);
		environment: (Dictionary new
			at: #FLGlobalVariableForTesting put: anObject;
			at: #Object put: Object;
			yourself).

	self deny: (self serializer globals includesKey: #FLGlobalVariableForTesting).
	self deny: (self resultOfSerializeAndMaterialize: anObject) identicalTo: anObject
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testGlobalClass [
	"A class should be global by default. On materialization, the global symbol should be found in the global environment."

	self materializer environment: (Dictionary new
		at: #Integer put: FLPerson;
		yourself).

	self assert: (self resultOfSerializeAndMaterialize: Integer) identicalTo: FLPerson
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testGlobalClassNotFound [
	"If a class is not defined in the materialization environment, a proper error should be raised."

	self serialize: Integer.
	
	self materializer environment: Dictionary new.
	
	self should: [ self materialized ] 
		raise: FLClassNotFound 
		description: 'Raise an error since the class is not present in materialization global environment'
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testGlobalMetaclass [
	"A metaclass should be global by default. On materialization, the global symbol should be found in the global environment."

	self materializer environment: (Dictionary new
		at: #Integer put: FLPerson;
		yourself).

	self assert: (self resultOfSerializeAndMaterialize: Integer class) identicalTo: FLPerson class
]

{ #category : 'tests' }
FLGlobalEnvironmentTest >> testGlobalMetaclassNotFound [
	"If a class is not defined in the materialization environment, a proper error should be raised."
	
	self serialize: Integer class.
	
	self materializer environment: Dictionary new.
	
	self should: [ self materialized ] 
		raise: FLClassNotFound 
		description: 'Raise an error since the class is not present in materialization global environment'
]
