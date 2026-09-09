"
I test the serialization of an entire class, in the cases in which the class *is not* present in the image at materialization time.
"
Class {
	#name : 'FLCreateClassSerializationTest',
	#superclass : 'FLClassSerializationTest',
	#traits : 'FLTCreateClassOrTraitSerializationTest',
	#classTraits : 'FLTCreateClassOrTraitSerializationTest classTrait',
	#category : 'Fuel-Core-Tests-FullSerialization',
	#package : 'Fuel-Core-Tests',
	#tag : 'FullSerialization'
}

{ #category : 'tests' }
FLCreateClassSerializationTest >> assertMaterializedHasCorrectFormat: aClass [
	| materializedClass |
	self serializer fullySerializeBehavior: aClass.
	
	materializedClass := self resultOfSerializeAndMaterialize: aClass.
	
	self deny: aClass identicalTo: materializedClass.
	self assert: aClass format equals: materializedClass format
]

{ #category : 'instance creation' }
FLCreateClassSerializationTest >> newAnonymousClassOrTrait [
	^ self classFactory newAnonymousClass
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testAnonymousObjectIsSerializable [
	| class |
	class := self classFactory newAnonymousClass.
	self deny: (self environmentOfTest includes: class).
	
	self serialize: class new.
	self
		shouldnt: [ self materialized ]
		raise: FLClassNotFound
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateByteArrayWithInstance [
	self flag: #fixIt. 
	"Tests internal materialization of ByteArray together with an instance of it.
	IMPORTANT: this test was disabled because it is dirty and gets broken classes/traits. 
	For more details see: http://code.google.com/p/pharo/issues/detail?id=6308"
	
	"| result |
	self analyzer considerInternalAllBehaviors: (ByteArray withAllSuperclasses removeAll: Collection withAllSuperclasses; yourself).
	
	result := self resultOfSerializeAndMaterialize: (ByteArray with: 42).
	
	self assert: result class name = #ByteArray.
	self assert: result size = 1.
	self assert: result first = 42."
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassAndMetaclass [
	"Tests materialization of a class or trait not defined in the image and that the metaclass is also created."

	| class materializedClass environment name metaclassName metaclass package packageTag |
	class := self classFactory silentlyNewClass.
	environment := class environment.
	package := class package.
	packageTag := class packageTag.
	name := class name.
	metaclassName := class class name.
	metaclass := class class.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: class.

	self assert: environment identicalTo: materializedClass environment.

	self assert: package identicalTo: materializedClass package.
	self assert: packageTag name equals: materializedClass packageTag name.
	self assert: name equals: materializedClass name.
	"Notice that the metaclass is serialized by Fuel and a new one will be created."
	self assert: metaclassName equals: materializedClass class name.
	self deny: metaclass identicalTo: materializedClass class.

	"It is important to notice that (at this moment) Fuel does not add the materialized class or trait into Smalltalk globals"
	self deny: (self environmentOfTest includesKey: name)
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithClassSideVariable [
	"Tests materialization a class not defined in the image, with a class-side instance variable."
	
	| aClass materializedClass |	
	aClass := self classFactory silentlyNewClass.
	aClass class addInstVarNamed: 'a'.
	aClass instVarNamed: 'a' put: #test.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.

	self assert: 1 = materializedClass class instVarNames size.
	self assert: (materializedClass class instVarNames includes: 'a').
	self assert: #test = (materializedClass instVarNamed: 'a').
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithClassVariable [
	"Tests materialization a class not defined in the image, with a class variable."

	| aClass materializedClass |
	aClass := self classFactory make: [ :aBuilder | aBuilder sharedVariables: #(ClassVariable) ].
	(aClass classVariableNamed: #ClassVariable) write: #test.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.

	self assert: 1 equals: materializedClass classVarNames size.
	self assert: (materializedClass classVarNames includes: #ClassVariable).
	self assert: #test equals: (materializedClass readClassVariableNamed: #ClassVariable).
	self assert: materializedClass classVariables first class identicalTo: ClassVariable
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithCreatedTrait [
	"Tests materialization of a class and a trait not defined in the image. The class uses the trait."

	| aClass aTrait result materializedClass materializedTrait |
	aClass := self classFactory silentlyNewClass.
	self classFactory silentlyCompile: 'fortyOne ^41' in: aClass.
	aTrait := self classFactory silentlyNewTrait.
	self classFactory silentlyCompile: 'fortyTwo ^42' in: aTrait.
	aClass addToComposition: aTrait.

	result := self resultOfSerializeRemoveAndMaterializeAll: (Array with: aClass with: aTrait).
	materializedClass := result first.
	materializedTrait := result second.
	self assert: result size equals: 2.
	self assert: materializedClass traits first identicalTo: materializedTrait.
	self assert: (materializedTrait users includes: materializedClass).
	self assert: materializedClass localSelectors size equals: 1.
	self assert: (materializedClass localSelectors includes: #fortyOne).
	self assert: materializedClass traitComposition transformations isNotEmpty
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithCreatedTraitWithInstanceVariable [
	"Tests materialization of a class and a trait not defined in the image.
	The class uses the trait.
	The trait has instance variables."

	| aClass aTrait result materializedClass materializedTrait materializedObject |
	aClass := self classFactory silentlyMake: [ :aBuilder | aBuilder slots: #(ivar) ].
	self classFactory silentlyCompile: 'ivar: x ivar := x' in: aClass.
	self classFactory silentlyCompile: 'ivar ^ ivar' in: aClass.
	aTrait := self classFactory silentlyMake: [ :aBuilder |
		          aBuilder
			          beTrait;
			          slots: #(traitIvar) ].
	self classFactory silentlyCompile: 'traitIvar: x traitIvar := x' in: aTrait.
	self classFactory silentlyCompile: 'traitIvar ^ traitIvar' in: aTrait.
	aClass addToComposition: aTrait.

	result := self resultOfSerializeRemoveAndMaterializeAll: {
			          aClass.
			          aTrait.
			          (aClass new
				           perform: #ivar: with: 1;
				           perform: #traitIvar: with: 2;
				           yourself) }.
	materializedClass := result first.
	materializedTrait := result second.
	materializedObject := result third.

	self assert: materializedClass slots size equals: 2.
	self assert: materializedTrait slots size equals: 1.
	self assert: materializedClass slots first name equals: 'ivar'.
	self assert: materializedClass slots second name equals: 'traitIvar'.
	self assert: (materializedObject perform: #ivar) equals: 1.
	self assert: (materializedObject perform: #traitIvar) equals: 2.

	"Check that the class format is correct and that the instance
	was allocated with enough space to store values."
	materializedObject
		perform: #ivar: with: 3;
		perform: #traitIvar: with: 4.
	self assert: (materializedObject perform: #ivar) equals: 3.
	self assert: (materializedObject perform: #traitIvar) equals: 4
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithInstance [
	"Tests materialization of a class who references an instance of itself."

	| aClass anInstance materializedClass materializedInstance |
	aClass := self classFactory make: [ :aBuilder | aBuilder slots: #(name) ].
	aClass class instanceVariableNames: 'instance'.

	anInstance := aClass new
		              instVarNamed: 'name' put: #testName;
		              yourself.
	aClass instVarNamed: 'instance' put: anInstance.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.
	materializedInstance := materializedClass instVarNamed: 'instance'.

	self deny: aClass == materializedClass.
	self deny: anInstance == materializedInstance.
	self assert: #testName = (materializedInstance instVarNamed: 'name')
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithSharedPool [
	"Tests materialization of a class not defined in the image, with a shared pool (NOTE: it will be an identity problem if it isn't a Smalltalk global')"
	
	| aClass materializedClass |	
	aClass := self classFactory silentlyNewClass.
	aClass addSharedPool: TextConstants.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.

	self assert: 1 = (materializedClass sharedPools size).
	self assert: TextConstants == materializedClass sharedPools first.
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithSharedPoolAndMethodReferencingAClassVariableOfIt [
	"Tests materialization of a class not defined in the image, with a shared pool and a compiled method that referers to a class side variable of the  sharedPool (NOTE: it will be an identity problem if it isn't a Smalltalk global')"

	| aClass materializedClass sharedPool anObject |
	aClass := self classFactory silentlyNewClass.
	anObject := Object new.

	sharedPool := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: SharedPool ].
	sharedPool addClassVarNamed: 'ClassVar1'.
	self classFactory
		silentlyCompile: 'classVar1: a  ClassVar1 := a.' in: sharedPool class;
		silentlyCompile: 'classVar1 ^  ClassVar1' in: sharedPool class.
	sharedPool perform: #classVar1: with: anObject.

	aClass addSharedPool: sharedPool.
	self classFactory
		silentlyCompile: 'xxx ^ ClassVar1.' in: aClass;
		silentlyCompile: 'yyy ^ ClassVar1.' in: aClass class.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.

	self assert: materializedClass sharedPools size equals: 1.
	self assert: sharedPool identicalTo: materializedClass sharedPools first.
	self assert: (materializedClass >> #xxx literalAt: 1) value identicalTo: (sharedPool readClassVariableNamed: #ClassVar1).
	self assert: (materializedClass class >> #yyy literalAt: 1) value identicalTo: (sharedPool readClassVariableNamed: #ClassVar1).
	self assert: (materializedClass new perform: #xxx) identicalTo: anObject.
	self assert: (materializedClass perform: #yyy) identicalTo: anObject
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithSharedPoolAndMethodReferencingIt [
	"Tests materialization of a class not defined in the image, with a shared pool and a compiled method that referers to an Association with such sharedPool (NOTE: it will be an identity problem if it isn't a Smalltalk global')"

	| aClass materializedClass sharedPool materializedMethod literalIndex |
	aClass := self classFactory silentlyNewClass.
	sharedPool := self classFactory make: [ :aBuilder | aBuilder superclass: SharedPool ].
	aClass addSharedPool: sharedPool.
	self classFactory silentlyCompile: 'initialize ' , sharedPool name asString , ' name.' in: aClass class.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.
	materializedMethod := materializedClass class >> #initialize.
	literalIndex := materializedMethod literals indexOf: (self environmentOfTest bindingOf: sharedPool name).

	self assert: materializedClass sharedPools size equals: 1.
	self assert: sharedPool identicalTo: materializedClass sharedPools first.
	self assert: (materializedMethod literalAt: literalIndex) value identicalTo: sharedPool
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateClassWithVariables [
	"Tests materialization of a class not defined in the image, with instance variables."

	| aClass materializedClass |
	aClass := self classFactory make: [ :aBuilder | aBuilder slots: #(var1 var2) ].

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.

	self assert: aClass instVarNames equals: materializedClass instVarNames
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateCompiledMethodClass [
	"Tests materialization of a CompiledMethod class that is not defined in the image"
	self assertMaterializedHasCorrectFormat: FLCompiledMethodClassMock
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateHierarchy [
	"Tests materialization of a hierarchy of classes that is not defined in the image"

	| serializedClasses materializedClasses a b c d |
	a := self classFactory silentlyNewClass.
	b := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: a ].
	c := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: a ].
	d := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: c ].
	serializedClasses := { a . b . c . d }.

	materializedClasses := self resultOfSerializeRemoveAndMaterializeAll: serializedClasses.

	self assert: serializedClasses size equals: materializedClasses size.

	serializedClasses withIndexDo: [ :aClass :index | self deny: aClass identicalTo: (materializedClasses at: index) ].

	self assert: Object equals: materializedClasses first superclass.
	self assert: materializedClasses first equals: materializedClasses second superclass.
	self assert: materializedClasses first equals: materializedClasses third superclass.
	self assert: materializedClasses third equals: materializedClasses fourth superclass.

	self assert: (Set with: materializedClasses second with: materializedClasses third) equals: materializedClasses first subclasses asSet.
	self assertEmpty: materializedClasses second subclasses.
	self assert: (Set with: materializedClasses fourth) equals: materializedClasses third subclasses asSet.
	self assertEmpty: materializedClasses fourth subclasses.
	"Notice that the instVar subclasses is only filled with classes inside the serialization. But for exernal classes Fuel does nothing. In this case,  FLStubA should NOT be added to Object subclasses."
	self deny: (Object subclasses includes: materializedClasses first).
	self deny: (Object subclasses identityIncludes: materializedClasses first)
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateHierarchyWithExistingClasses [
	"Tests materialization of a hierarchy of classes that are not defined in the image and classes that indeed are defined"

	| serializedClasses materializedClasses a b c d |
	a := self classFactory silentlyNewClass.
	b := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: a ].
	c := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: a ].
	d := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: Date ].
	serializedClasses := { a . b . c . d }.

	materializedClasses := self resultOfSerializeRemoveAndMaterializeAll: serializedClasses.

	self assert: serializedClasses size equals: materializedClasses size.

	serializedClasses withIndexDo: [ :aClass :index | self deny: aClass identicalTo: (materializedClasses at: index) ].
	self assert: Object identicalTo: materializedClasses first superclass.
	self assert: materializedClasses first identicalTo: materializedClasses second superclass.
	self assert: materializedClasses first identicalTo: materializedClasses third superclass.
	"Notice that the superclass of d is Date and since it is present in the image, we need exactly that instance."
	self assert: Date identicalTo: materializedClasses fourth superclass.

	self assert: (Set with: materializedClasses second with: materializedClasses third) equals: materializedClasses first subclasses asSet.
	self assertEmpty: materializedClasses second subclasses.
	self assertEmpty: materializedClasses third subclasses.
	self assertEmpty: materializedClasses fourth subclasses.

	self deny: (Object subclasses identityIncludes: materializedClasses first).
	self deny: (Date subclasses identityIncludes: materializedClasses fourth)
]

{ #category : 'tests-bugs' }
FLCreateClassSerializationTest >> testCreateHierarchyWithSubclassSerializedBeforeSuperclass [
	"Tests issue #210
	See FLBehaviorCluster>>registerIndexesOn:"

	| a b serializedClasses |
	a := self classFactory silentlyNewClass.
	b := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: a ].
	serializedClasses := { a . b }.

	self shouldnt: [ self resultOfSerializeRemoveAndMaterializeAll: serializedClasses ] raise: MessageNotUnderstood.
	
	serializedClasses := { b. a}.
	self shouldnt: [ self resultOfSerializeRemoveAndMaterializeAll: serializedClasses ] raise: MessageNotUnderstood
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateNormalClass [
	"Tests materialization of a class that is not defined in the image"
	self assertMaterializedHasCorrectFormat: FLPair
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateSharedPoolClass [
	"Tests materialization of a shared pool class not defined in the image (NOTE: it will be an identity problem if it isn't a Smalltalk global')"

	| materializedClass sharedPool |
	sharedPool := self classFactory make: [ :aBuilder | aBuilder superclass: SharedPool ].
	sharedPool addClassVarNamed: 'ClassVar1'.
	self classFactory
		silentlyCompile: 'classVar1: a  ClassVar1 := a.' in: sharedPool class;
		silentlyCompile: 'classVar1 ^  ClassVar1 ' in: sharedPool class.

	sharedPool perform: #classVar1: with: #foo.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: sharedPool.

	self assert: (materializedClass perform: #classVar1) equals: #foo
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateVariableByteClass [
	"Tests materialization of a variable byte class that is not defined in the image"
	self assertMaterializedHasCorrectFormat: FLVariableByteClassMock
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateVariableClass [
	"Tests materialization of a variable class that is not defined in the image"
	self assertMaterializedHasCorrectFormat: FLVariableClassMock
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateVariableWordClass [
	"Tests materialization of a variable word class that is not defined in the image"
	self assertMaterializedHasCorrectFormat: FLVariableWordClassMock
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateWeakClass [
	"Tests materialization of a weak class that is not defined in the image"
	self assertMaterializedHasCorrectFormat: FLWeakClassMock
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateWithClassSideInitializeMethod [
	"Tests materialization of a class-side #initialize method in a class or trait not defined in the image. It makes sure that Fuel doesn't call it automatically"

	| aClassOrTrait materializedClassOrTrait category |
	category := 'tests-initialize'.
	aClassOrTrait := self classFactory silentlyNewClass.
	self classFactory
		silentlyCompile: 'initialize
			self error: ''Fuel should not automatically call #initialize'''
		in: aClassOrTrait classSide
		protocol: category.

	self
		shouldnt: [ materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClassOrTrait ]
		raise: Error.
	self
		should: [ (self newInstanceFrom: materializedClassOrTrait) class perform: #initialize ]
		raise: Error
		description: 'Fuel should not automatically call #initialize'.

	self assert: (materializedClassOrTrait classSide includesSelector: #initialize).
	self assertCollection: #(initialize) hasSameElements: materializedClassOrTrait classSide localSelectors.
	self assert: category equals: (materializedClassOrTrait classSide protocolNameOfSelector: #initialize)
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateWithClassVariableAccessors [
	"Tests materialization of a compiled method in a class not defined in the image. The class defines accessors for a class variable."

	| aClass materializedClass instance |
	aClass := self classFactory silentlyMake: [ :aBuilder | aBuilder sharedVariables: #(AnFLClassVariable) ].
	self classFactory
		silentlyCompile: 'classVariable ^AnFLClassVariable' in: aClass;
		silentlyCompile: 'classVariable: value AnFLClassVariable := value' in: aClass.
	(self newInstanceFrom: aClass) classVariable: 42.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: aClass.
	instance := self newInstanceFrom: materializedClass.

	self assert: (materializedClass includesSelector: #classVariable).
	self assert: 42 equals: instance classVariable.
	instance classVariable: 0.
	self assert: 0 equals: instance classVariable
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateWithNilEnvironment [ 
	"Tests materialization a class or trait not defined in the image, and with a nil in the instance variable 'environment ' "

	| aClassOrTrait materializedClassOrTrait |
	aClassOrTrait := self classFactory silentlyNewClass.
	aClassOrTrait environment: nil.
	
	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClassOrTrait.

	self assert:  materializedClassOrTrait basicEnvironment isNil.
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateWithSmalltalkGlobalsEnvironment [
	"Tests materialization of a class or trait not defined in the image, and with Smalltalk globals  in the instance variable 'environment'. In that case, the whole Smalltalk globals should NOT be fully serialized but only a reference to the globals should be stored. "

	| class materializedClass name package packageTag |
	class := self classFactory silentlyNewClass.
	class environment: Smalltalk globals.
	package := class package.
	packageTag := class packageTag.
	name := class name.

	materializedClass := self resultOfSerializeRemoveAndMaterialize: class.

	self assert: materializedClass basicEnvironment identicalTo: Smalltalk globals.

	self assert: package identicalTo: materializedClass package.
	self assert: packageTag name equals: materializedClass packageTag name.
	self assert: name equals: materializedClass name
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testCreateWithSubclass [
	"Tests materialization of a class that has a external subclass. Currently, this is the expected behaviour."

	| serializedClass aSubclass materializedClass |
	serializedClass := self classFactory silentlyNewClass.
	aSubclass := self classFactory silentlyMake: [ :aBuilder | aBuilder superclass: serializedClass ].

	materializedClass := self resultOfSerializeRemoveAndMaterialize: serializedClass.
	self assert: materializedClass subclasses size equals: 1.
	self assert: materializedClass subclasses first identicalTo: aSubclass
]

{ #category : 'tests' }
FLCreateClassSerializationTest >> testMaterializationDoesNotModifySerializedClass [
	"Tests that materialization does not change the source class."

	| aClass materializedClass |
	aClass := self classFactory make: [ :aBuilder | aBuilder slots: #(var) ].

	self serializer fullySerializeBehavior: aClass.
	self serialize: aClass.
	aClass removeInstVarNamed: 'var'.
	materializedClass := self materialized.

	self assert: materializedClass instVarNames = #( var ) description: 'The materialized class has the variable as the serialized class'.
	self assert: aClass instVarNames isEmpty description: 'Materialization should not modify the serialized class'
]

{ #category : 'tests-bugs' }
FLCreateClassSerializationTest >> testSerializeAnonymousSubclass [
	| anonymous |
	anonymous := self classFactory newAnonymousClass.
	
	self
		shouldnt: [ self resultOfSerializeRemoveAndMaterializeAll: {anonymous} ]
		raise: FLObjectNotFound
]
