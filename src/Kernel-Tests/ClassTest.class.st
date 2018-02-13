"
SUnit tests for classes
" 
Class {
	#name : #ClassTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'className',
		'renamedName'
	],
	#category : #'Kernel-Tests-Classes'
}

{ #category : #setup }
ClassTest >> categoryNameForTemporaryClasses [
	"Answer the category where to classify temporarily created classes"

	^'Dummy-Tests-Class' 
]

{ #category : #setup }
ClassTest >> deleteClass [
	| cl |
	cl := Smalltalk globals at: className ifAbsent: [ ^ self ].
	cl
		removeFromChanges;
		removeFromSystemUnlogged
]

{ #category : #'referencing methods' }
ClassTest >> referencingMethod1 [

	^ ExampleForTest1
]

{ #category : #'referencing methods' }
ClassTest >> referencingMethod2 [

	^ {ExampleForTest12. ExampleForTest1}
]

{ #category : #'referencing methods' }
ClassTest >> referencingMethod3 [
	"no reference"

	^ self
]

{ #category : #running }
ClassTest >> setUp [
	super setUp.
	className := #TUTU.
	self deleteClass.
	Object subclass: className
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self categoryNameForTemporaryClasses
]

{ #category : #running }
ClassTest >> tearDown [
	self deleteClass.
	{self unclassifiedCategory. self categoryNameForTemporaryClasses} do: [:category|
			RPackage organizer unregisterPackageNamed: category].
	super tearDown	
]

{ #category : #tests }
ClassTest >> testAddClassSlot [

	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu addClassSlot: #'X' => InstanceVariableSlot.
	self assert: tutu class instVarNames = #(#'X').
	tutu addClassSlot: #'Y' => InstanceVariableSlot.
	self assert: tutu class instVarNames = #(#'X' #'Y')
]

{ #category : #tests }
ClassTest >> testAddInstVarName [

	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu addInstVarNamed: 'x'.
	self assert: tutu instVarNames equals: #('x').
	tutu addInstVarNamed: 'y'.
	self assert: tutu instVarNames equals: #('x' 'y')
]

{ #category : #tests }
ClassTest >> testAddSlot [

	| tutu |
	tutu := Smalltalk globals at: #TUTU.
	tutu addSlot: #x => InstanceVariableSlot.
	self assert: tutu instVarNames equals: #('x').
	self assert: tutu name equals: #TUTU.
	tutu addSlot: #y => InstanceVariableSlot.
	self assert: tutu instVarNames equals: #('x' 'y')
]

{ #category : #tests }
ClassTest >> testAddSlotAnonymous [

	| tutu |
	tutu := Object newAnonymousSubclass.
	self assert: tutu getName isNil.
	tutu := tutu addSlot: #x => InstanceVariableSlot.
	self assert: tutu instVarNames equals: #('x').
	self assert: tutu getName isNil.
	tutu := tutu addSlot: #y => InstanceVariableSlot.
	self assert: tutu getName isNil.
	self assert: tutu instVarNames equals: #('x' 'y')
]

{ #category : #'tests - access' }
ClassTest >> testAllSharedPools [

	self assert: Point allSharedPools equals: OrderedCollection new.
	self assert: Date sharedPools first equals: ChronologyConstants.
	self assert: Date sharedPools size equals: 1.	"a metaclass does not have shared pools since only classes have shared pools"

	self assert: RootClassPoolUser sharedPools size equals: 1.
	self assert: ClassMultiplePoolUser sharedPools size equals: 2.	"has shared pools does not take into account the fact that a superclass may use some shared pools"
	self assert: SubclassPoolUser sharedPools isEmpty
]

{ #category : #'tests - class variables' }
ClassTest >> testClassVarNames [

	self assert: (Object classVarNames includes: #DependentsFields).
	
	"A class and it's meta-class share the class variables"
	self assert: (Object classVarNames = Object class classVarNames).
]

{ #category : #'tests - dependencies' }
ClassTest >> testDependencies [
	self assert: (ClassTest dependentClasses includes: ClassTest superclass).
	self assert: (ClassTest dependentClasses includes: Date)
]

{ #category : #'tests - access' }
ClassTest >> testHasPoolVarNamed [	

	self assert: (Date usesLocalPoolVarNamed: 'DayNames').
	"a metaclass does not have shared pools since only classes have shared pools"
	self deny: (Date class usesLocalPoolVarNamed: 'DayNames').
	
	self assert: (RootClassPoolUser usesLocalPoolVarNamed: 'Author').
	"a subclass does not have the one of its superclass - but it would be good to change that"
	self deny: (SubclassPoolUser usesLocalPoolVarNamed: 'Author').
]

{ #category : #'tests - access' }
ClassTest >> testHasSharedPools [

	self deny: Point hasSharedPools.
	self assert: Date hasSharedPools.
	
	"a metaclass does not have shared pools since only classes have shared pools"
	self deny: Date class hasSharedPools.
	
	self assert: RootClassPoolUser hasSharedPools.
	
	"has shared pools does not take into account the fact that a superclass may use some shared pools"
	self deny: SubclassPoolUser hasSharedPools.
]

{ #category : #'tests - navigation' }
ClassTest >> testMethodsReferencingClass [
	self assert: (ClassTest methodsReferencingClass: (Smalltalk classNamed: #ExampleForTest111)) equals: {(ClassTest >> #testOrdersACollectionOfClassesBySuperclass)}.
	self
		assert: ((ClassTest methodsReferencingClass: (Smalltalk classNamed: #ExampleForTest1)) sort: [ :a :b | a name <= b name]) asArray
		equals:
			{ClassTest>>#referencingMethod1. 
			ClassTest>>#referencingMethod2. 
			ClassTest>>#testOrdersACollectionOfClassesBySuperclass}.
	self assert: (ClassTest methodsReferencingClass: (Smalltalk classNamed: #BehaviorTest)) isEmpty
]

{ #category : #'tests - navigation' }
ClassTest >> testMethodsReferencingClasses [

	| collectionOfMethods collectionOfMethodsShouldBe |
	
	collectionOfMethods := ((ClassTest methodsReferencingClasses: {Smalltalk classNamed: #ExampleForTest12. Smalltalk classNamed: #ExampleForTest1}) sort: [ :a :b | a name <= b name]) asArray.
	
	collectionOfMethodsShouldBe := {
		ClassTest>>#referencingMethod1.
		ClassTest>>#referencingMethod2.
		ClassTest>>#testOrdersACollectionOfClassesBySuperclass}.
		
	self assert: collectionOfMethods asSet equals: collectionOfMethodsShouldBe asSet
]

{ #category : #'tests - class creation' }
ClassTest >> testNewSubclass [
	| cls |
	cls := Point newSubclass.
	self assert: (cls isBehavior).
	self assert: (cls superclass == Point).
	self assert: (Point allSubclasses includes: cls).
	self assert: (cls instVarNames = #()).
	self assert: (cls category = self unclassifiedCategory).
	self assert: (cls classVarNames = #()).
	
	cls removeFromSystem.
]

{ #category : #'tests - file in/out' }
ClassTest >> testOrdersACollectionOfClassesBySuperclass [	
	| ordered |
	ordered := (Class superclassOrder:  
		(OrderedCollection 
				with: ExampleForTest11 class 
				with: ExampleForTest111 class 
				with: ExampleForTest12 class 
				with: ExampleForTest1 class 
				with: ExampleForTest12 class 
				with: ExampleForTest112 class)).
	
	self assert: (ordered indexOf: ExampleForTest1 class) < (ordered indexOf: ExampleForTest11 class).
	self assert: (ordered indexOf: ExampleForTest11 class) < (ordered indexOf: ExampleForTest111 class).
	self assert: (ordered indexOf: ExampleForTest11 class) < (ordered indexOf: ExampleForTest112 class).
	self assert: (ordered indexOf: ExampleForTest1 class) < (ordered indexOf: ExampleForTest12 class).
	
]

{ #category : #'tests - file in/out' }
ClassTest >> testOrdersMetaClassAfterItsClassInstance [
	| ordered |
	ordered := (Class superclassOrder:  
		(OrderedCollection 
				with: Boolean class
				with: True
				with: Boolean
				with: True class)).
	
	self assert: (ordered indexOf: Boolean) < (ordered indexOf: Boolean class).
	self assert: (ordered indexOf: True) < (ordered indexOf: True class).
	self assert: (ordered indexOf: Boolean class) < (ordered indexOf: True class).
	self assert: (ordered indexOf: Boolean) < (ordered indexOf: True).

]

{ #category : #'tests - navigation' }
ClassTest >> testReferencedClasses [
	{(ExceptionTester -> { MyTestNotification. Warning. String. MyResumableTestError. OrderedCollection. MyTestError}). 
	 (CollectionCombinator -> {Array}).
	 (ExecutionEnvironmentStub -> {OrderedCollection})
	}
		do: [ :assoc | 
			self assert: assoc key referencedClasses notEmpty.
			self assert: (assoc key referencedClasses asSet includesAll: assoc value asSet)]
]

{ #category : #'tests - access' }
ClassTest >> testSharedPoolOfVarNamed [	

	self assert: (Date sharedPoolOfVarNamed: 'DayNames') = ChronologyConstants.
	"a metaclass does not have shared pools since only classes have shared pools"
	self assert: (Date class sharedPoolOfVarNamed: 'DayNames') isNil.
	
	self assert: (RootClassPoolUser sharedPoolOfVarNamed: 'Author') = PoolDefiner.
	self assert: (RootClassPoolUser sharedPoolOfVarNamed: 'Gloups') = PoolDefiner.
	self assert: (SubclassPoolUser sharedPoolOfVarNamed: 'Author') = PoolDefiner.
	
	self assert: (ClassMultiplePoolUser sharedPoolOfVarNamed: 'Author') = PoolDefiner.
	self assert: (ClassMultiplePoolUser sharedPoolOfVarNamed: 'VariableInPoolDefiner2') = PoolDefiner2.
	self assert: (ClassMultiplePoolUser sharedPoolOfVarNamed: 'Gloups') = PoolDefiner.
]

{ #category : #'tests - access' }
ClassTest >> testSharedPools [

	self assert: Point sharedPools equals: OrderedCollection new.
	self assert: Date sharedPools first equals: ChronologyConstants.
	self assert: Date sharedPools size equals: 1.	"a metaclass does not have shared pools since only classes have shared pools"
	Date class sharedPools.
	self assert: RootClassPoolUser sharedPools size equals: 1.
	self assert: ClassMultiplePoolUser sharedPools size equals: 2.	"has shared pools does not take into account the fact that a superclass may use some shared pools"
	self assert: SubclassPoolUser sharedPools isEmpty
]

{ #category : #'tests - class creation' }
ClassTest >> testSubclass [
	| cls |
	(Smalltalk globals includesKey: #SubclassExample)
		ifTrue: [ (Smalltalk at: #SubclassExample) removeFromSystem ].
	
	self deny: (Smalltalk globals includesKey: #SubclassExample).
	cls := Object subclass: #SubclassExample.
	self assert: (Smalltalk globals includesKey: #SubclassExample).

	self assert: (Smalltalk at: #SubclassExample) == cls.
	self assert: cls category = #Unclassified.
	self assert: cls instVarNames = #().
	
	cls removeFromSystem
]

{ #category : #'tests - class creation' }
ClassTest >> testSubclassInstanceVariableNames [
	| cls |
	(Smalltalk globals includesKey: #SubclassExample)
		ifTrue: [ (Smalltalk at: #SubclassExample) removeFromSystem ].
	
	self deny: (Smalltalk globals includesKey: #SubclassExample).
	cls := Object subclass: #SubclassExample instanceVariableNames: 'x y'.
	self assert: (Smalltalk globals includesKey: #SubclassExample).

	self assert: (Smalltalk at: #SubclassExample) == cls.
	self assert: cls category = #Unclassified.
	self assert: cls instVarNames = #('x' 'y').
	
	cls removeFromSystem
]

{ #category : #'tests - file in/out' }
ClassTest >> testSuperclassOrder [
	|  ordered orderedSuperclasses shuffledSuperclasses |
	orderedSuperclasses := {ProtoObject. Object. Collection. SequenceableCollection}.
	
	"a shuffled collection of superclasses of OrderedCollection"
	shuffledSuperclasses := {Collection. SequenceableCollection. ProtoObject. Object}.
	
	ordered := Class superclassOrder: shuffledSuperclasses.
	
	"should not affect the order as there is no dependencies"
	self assert: ordered = orderedSuperclasses asOrderedCollection.
]

{ #category : #'tests - file in/out' }
ClassTest >> testSuperclassOrderPreservingOrder [
	| noHierarchicalRelationship ordered |
	"a shuffled collection of direct subclasses of Collection"
	noHierarchicalRelationship := {CharacterSet. WideCharacterSet. OrderedDictionary. DependentsArray. Bag. SmallDictionary. SequenceableCollection. HashedCollection. WeakRegistry. Matrix. Heap}.
	
	ordered := Class superclassOrder: noHierarchicalRelationship.
	
	"should not affect the order as there is no dependencies"
	self assert: ordered = noHierarchicalRelationship asOrderedCollection.
	
	
]

{ #category : #'tests - access' }
ClassTest >> testUsesPoolVarNamed [	

	self assert: (Date usesPoolVarNamed: 'DayNames').
	"a metaclass does not have shared pools since only classes have shared pools"
	self deny: (Date class usesPoolVarNamed: 'DayNames').
	
	self assert: (RootClassPoolUser usesPoolVarNamed: 'Author').
	"a subclass  has  the one of its superclass"
	self assert: (SubclassPoolUser usesPoolVarNamed: 'Author').
]

{ #category : #'tests - class creation' }
ClassTest >> unclassifiedCategory [
	^#Unclassified
]
