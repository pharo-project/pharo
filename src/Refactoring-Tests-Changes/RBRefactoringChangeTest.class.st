Class {
	#name : #RBRefactoringChangeTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'changes',
		'workingEnvironment',
		'changeFactory'
	],
	#category : #'Refactoring-Tests-Changes'
}

{ #category : #accessing }
RBRefactoringChangeTest class >> packageNamesUnderTest [
	^ #('Refactoring-Changes')
]

{ #category : #mocking }
RBRefactoringChangeTest >> changeMock [
	^ testingEnvironment at: #RBRefactoringChangeMock
]

{ #category : #mocking }
RBRefactoringChangeTest >> createMockClass [
	self class compiler 
		evaluate: 'Object subclass: #RBRefactoringChangeMock
	instanceVariableNames: ''instVar''
	classVariableNames: ''ClassVar''
	poolDictionaries: ''''
	category: ''Refactoring-Tests-Changes'''.
	
	self class compiler 
		evaluate:  'RBRefactoringChangeMock class
	instanceVariableNames: ''classInstVar'''.
	
	self changeMock compile: 'one ^ 1' classified: 'accessing'.
]

{ #category : #utilities }
RBRefactoringChangeTest >> equalityTestFor: aChange [
	self assert: aChange equals: aChange.
	self assert: aChange hash equals: aChange hash.

	self assert: aChange copy equals: aChange.
	self assert: aChange copy hash equals: aChange hash
]

{ #category : #accessing }
RBRefactoringChangeTest >> exampleClasses [
	<sampleInstance>
	^ { "Standard Classes" 
		ProtoObject. Object. Class. Metaclass. Behavior. ClassDescription. Dictionary. Trait.
		
		"Lots of class vars"
		SmalltalkImage
	}
]

{ #category : #accessing }
RBRefactoringChangeTest >> exampleTraits [
    <sampleInstance>
	^ { TSortable. TAssertable }
]

{ #category : #utilities }
RBRefactoringChangeTest >> perform: aChange do: aBlock [
	"Perform a change in the system silently, evaluate aBlock and then undo the change again."

	| undo |
	^ SystemAnnouncer uniqueInstance suspendAllWhile: [
		undo := aChange execute.
		aBlock ensure: [ undo execute ] ]
]

{ #category : #mocking }
RBRefactoringChangeTest >> removeMockClass [
	testingEnvironment removeClassNamed: #RBRefactoringChangeMock.
]

{ #category : #mocking }
RBRefactoringChangeTest >> selectionInterval [
	^ 1 to: 0
]

{ #category : #running }
RBRefactoringChangeTest >> setUp [
	super setUp.
	workingEnvironment := Smalltalk globals.
	"In the future we should make sure that the tests can run on a new environment."
	self createMockClass.
	changeFactory := RBRefactoryChangeManager changeFactory.
	"changeFactory is used in tests too"
	changes := changeFactory compositeRefactoryChangeNamed: 'testing'
]

{ #category : #utilities }
RBRefactoringChangeTest >> stringTestFor: aChange [
	self assert: (aChange name isString and: [ aChange name notEmpty ]).
	self assert: (aChange printString isString and: [ aChange printString notEmpty ]).
	self assert: (aChange changeString isString and: [ aChange changeString notEmpty ]).
	self assert: (aChange displayString isString and: [ aChange displayString notEmpty ])
]

{ #category : #running }
RBRefactoringChangeTest >> tearDown [
	self removeMockClass.
	super tearDown.
]

{ #category : #tests }
RBRefactoringChangeTest >> testAddClassInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'instVar' to: self class class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class class.
	self assert: change isMeta.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testAddClassInteractively [
	| change |
	change := changeFactory addClassDefinition: 'AbstractEnvironmentTestCase subclass: #' , self class name , '
	instanceVariableNames: ''instVar''
	classVariableNames: ''ClassVar''
	poolDictionaries: ''PoolDict''
	package: ''' , self class category , '''' for: self.
	self assert: change controller equals: self.
	self assert: change superclassName equals: self class superclass name.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change instanceVariableNames size equals: 1.
	self assert: change instanceVariableNames first equals: 'instVar'.
	self assert: change classVariableNames size equals: 1.
	self assert: change classVariableNames first equals: 'ClassVar'.
	self assert: change sharedPoolNames size equals: 1.
	self assert: change sharedPoolNames first equals: 'PoolDict'.
	self assert: change category equals: self class category.
	self universalTestFor: change
]

{ #category : #'tests-pattern' }
RBRefactoringChangeTest >> testAddClassPattern [

	"Make sure that all class definitions can be parsed."

	self exampleClasses
		do: [ :class | 
			( class isObsolete or: [ class superclass notNil and: [ class superclass isObsolete ] ] )
				ifFalse: [ | change |

					change := changes defineClass: class oldDefinition.
					self assert: ( change isKindOf: RBAddClassChange ).
					self assert: change changeClassName equals: class name.

					class superclass
						ifNil: [ self assert: change definitionClass equals: ProtoObject ]
						ifNotNil: [ self assert: change definitionClass equals: class superclass ].

					self assert: change instanceVariableNames equals: class instVarNames asArray.
					self assert: change classVariableNames equals: class classVarNames asArray.
					self assert: change sharedPoolNames equals: class sharedPoolNames asArray.
					self assert: change category equals: class category.
					self universalTestFor: change
					]
			]
]

{ #category : #'tests-pattern' }
RBRefactoringChangeTest >> testAddClassTraitPattern [
	"Make sure that all class trait definitions can be parsed."
	
	self exampleTraits do: [ :trait |
		| change |
		change := changes defineClass: trait classTrait definition.
		self assert: (change isKindOf: RBAddClassTraitChange).
		self assert: change changeClassName equals: trait name.
		self universalTestFor: change ]
]

{ #category : #tests }
RBRefactoringChangeTest >> testAddClassVariable [
	| change |
	change := changes addClassVariable: 'ClassVar' to: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'ClassVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testAddInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'instVar' to: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : #'tests-pattern' }
RBRefactoringChangeTest >> testAddMetaclassPattern [
	"Make sure that metaclass definitions can be parsed."
	
	self exampleClasses do: [ :class |
		(class isObsolete or: [ class superclass notNil and: [ class superclass isObsolete ] ]) ifFalse: [  
 		| change |
		change := changes defineClass: class class definition.
		self assert: (change isKindOf: RBAddMetaclassChange).
		self assert: change changeClassName equals: class name.
		self assert: change classInstanceVariableNames equals: class class instVarNames.
		self universalTestFor: change ]]
]

{ #category : #tests }
RBRefactoringChangeTest >> testAddPool [
	| change |
	change := changes addPool: 'PoolDict' to: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'PoolDict'.
	self universalTestFor: change
]

{ #category : #'tests-pattern' }
RBRefactoringChangeTest >> testAddTraitPattern [
	"Make sure that all trait definitions can be parsed."
	
	self exampleTraits do: [ :trait |
		| change |
		change := changes defineClass: trait definition.
		self assert: (change isKindOf: RBAddTraitChange).
		self assert: change changeClassName equals: trait name.
		self assert: change category equals: trait category.
		self universalTestFor: change ]
]

{ #category : #tests }
RBRefactoringChangeTest >> testComment [
	| change |
	change := changes comment: 'Some Comment' in: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change comment equals: 'Some Comment'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testCompileInClass [
	| change |
	change := changes compile: 'setUp' in: self class.
	self assert: change controller isNil.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self assert: change source equals: 'setUp'.
	self assert: change protocol equals: #running.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testCompileInClassified [
	| change |
	change := changes compile: 'setUp' in: self class classified: #accessing.
	self assert: change controller isNil.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self assert: change source equals: 'setUp'.
	self assert: change protocol equals: #accessing.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testCompileInInteractively [
	| change |
	change := changeFactory addMethodSource: 'setUp' in: self class classified: #running for: self.
	self assert: change controller equals: self.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self assert: change source equals: 'setUp'.
	self assert: change protocol equals: #running.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testCompileInMetaclass [
	| change |
	change := changes compile: 'new' in: self class class.
	self assert: change controller isNil.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class class.
	self assert: change isMeta.
	self assert: change selector equals: #new.
	self assert: change source equals: 'new'.
	self assert: change protocol equals: #accessing.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testComposite [
	changes 
		compile: 'method ^ 1' in: self class classified: #utilities;
		compile: 'method ^ 2' in: self class class classified: #utilities.
	self assert: changes changesSize equals: 2.
	self assert: changes problemCount equals: 2.
	self assert: (changes changeForClass: self class name selector: #method) notNil.
	self assert: (changes changeForMetaclass: self class name selector: #method) notNil.
	self universalTestFor: changes
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClass [
	| change |
	change := changes defineClass: 'Object subclass: #' , self changeMock name , 'Temporary
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	package: ''' , self class category , ''''.
	self perform: change do: [
		self assert: (workingEnvironment  hasClassNamed: change changeClassName).
		self assert: change definedClass name equals: change changeClassName.
		self assert: change definedClass isBehavior ].
	self deny: (workingEnvironment hasClassNamed: change changeClassName).
	self assert: change definedClass isObsolete
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'foo' to: self changeMock class.
	self perform: change do: [ self assert: (change changeClass instVarNames includes: 'foo') ].
	self deny: (change changeClass instVarNames includes: 'foo')
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassInteractively [
	| change |
	change := changeFactory addClassDefinition: 'Object subclass: #' , self changeMock name , 'Temporary
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	package: ''' , self class category , '''' for: self.
	self perform: change do: [ 
		self assert: (workingEnvironment hasClassNamed: change changeClassName).
		self assert: change definedClass name equals: change changeClassName.
		self assert: change definedClass isBehavior ].
	self deny: (workingEnvironment hasClassNamed: change changeClassName).
	self assert: change definedClass isObsolete
	
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassMethod [
	| change |
	change := changes compile: 'method ^ 1' in: self changeMock class classified: #utilities.
	self perform: change do: [ self assert: (self changeMock respondsTo: #method) ].
	self deny: (self changeMock respondsTo: #method).
	self assert: change definedSelector equals: #method
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassVariable [
	| change |
	change := changes addClassVariable: 'Foo' to: self changeMock.
	self perform: change do: [ self assert: (change changeClass classVarNames includes: 'Foo') ].
	self deny: (change changeClass classVarNames includes: 'Foo')
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'foo' to: self changeMock.
	self perform: change do: [ self assert: (change changeClass instVarNames includes: 'foo') ].
	self deny: (change changeClass instVarNames includes: 'foo')
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveMethod [
	| change |
	change := changes compile: 'method ^ 1' in: self changeMock classified: #utilities.
	self perform: change do: [ self assert: (self changeMock canUnderstand: #method) ].
	self deny: (self changeMock canUnderstand: #method).
	self assert: change definedSelector equals: #method
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformAddRemoveMethodInteractively [
	| change |
	change := changeFactory addMethodSource: 'method ^ 1' in: self changeMock classified: #utilities for: self. 
	self perform: change do: [ self assert: (self changeMock canUnderstand: #method) ].
	self deny: (self changeMock canUnderstand: #method).
	self assert: change definedSelector equals: #method
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformChangeClass [
	| change |
	change := changes
		defineClass:
			self class name , ' subclass: #' , self changeMock name
				,
					'
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , self class category , ''''.
	self
		perform: change
		do: [ self assert: change changeClass superclass equals: self class.
			self assertEmpty: change changeClass instVarNames ].
	self assert: change changeClass superclass equals: Object.
	self denyEmpty: change changeClass instVarNames.
	self assert: change definedClass equals: self changeMock
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformChangeComment [
	| change comment |
	change := changes comment: 'Some Comment' in: self changeMock.
	comment := change changeClass organization classComment.
	self perform: change do: [ self assert: change changeClass comment equals: 'Some Comment' ].
	self assert: change changeClass organization classComment equals: comment
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformChangeMetaclass [
	| change |
	change := changes
		defineClass:
			self changeMock name
				,
					' class 
	instanceVariableNames: '''''.
	self perform: change do: [ self assertEmpty: change changeClass class instVarNames ].
	self denyEmpty: change changeClass class instVarNames.
	self assert: change definedClass equals: self changeMock class
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformChangeMethod [
	| change source |
	change := changes compile: 'one ^ 2' in: self changeMock.
	source := change changeClass sourceCodeAt: #one.
	self perform: change do: [ self assert: (change changeClass sourceCodeAt: #one) equals: 'one ^ 2' ].
	self assert: (change changeClass sourceCodeAt: #one) equals: source
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformCompositeChange [
	changes 
		compile: 'method1 ^ 1' in: self changeMock;
		compile: 'method2 ^ 2' in: self changeMock.
	self perform: changes do: [ 
		self assert: (self changeMock canUnderstand: #method1).
		self assert: (self changeMock canUnderstand: #method2) ].
	self deny: (self changeMock canUnderstand: #method1).
	self deny: (self changeMock canUnderstand: #method2) 
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformRenameClass [
	| change |
	change := changes renameClass: self changeMock to: self changeMock name , 'Plus'.
	self perform: change do: [ 
		self deny: (workingEnvironment hasClassNamed: change oldName).
		self assert: (workingEnvironment hasClassNamed: change newName) ].
	self assert: (workingEnvironment hasClassNamed: change oldName).
	self deny: (workingEnvironment hasClassNamed: change newName)
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformRenameClassInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'classInstVar' to: 'classInstVarPlus' in: self changeMock class.
	self perform: change do: [ 
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformRenameClassVariable [
	| change |
	change := changes renameClassVariable: 'ClassVar' to: 'ClassVarPlus' in: self changeMock.
	self perform: change do: [ 
		self deny: (change changeClass classVarNames includes: change oldName).
		self assert: (change changeClass classVarNames includes: change newName) ].
	self assert: (change changeClass classVarNames includes: change oldName).
	self deny: (change changeClass classVarNames includes: change newName)
]

{ #category : #'tests-perform' }
RBRefactoringChangeTest >> testPerformRenameInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'instVar' to: 'instVarPlus' in: self changeMock.
	self perform: change do: [ 
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)
]

{ #category : #tests }
RBRefactoringChangeTest >> testRemoveClass [
	| change |
	change := changes removeClass: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRemoveClassInstanceVariable [
	| change |
	change := changes removeInstanceVariable: 'instVar' from: self class class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class class.
	self assert: change isMeta.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRemoveClassNamed [
	| change |
	change := changes removeClassNamed: self class name.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRemoveClassVariable [
	| change |
	change := changes removeClassVariable: 'ClassVar' from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'ClassVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRemoveInstanceVariable [
	| change |
	change := changes removeInstanceVariable: 'instVar' from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRemoveMethod [
	| change |
	change := changes removeMethod: #setUp from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRemovePool [
	| change |
	change := changes removePool: 'PoolDict' from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'PoolDict'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRenameClass [
	| change |
	change := changes renameClass: self class to: self class name , 'Plus'.
	self assert: change oldName equals: self class name.
	self assert: change newName equals: (self class name , 'Plus').
	self assert: change changeClass equals: self class.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRenameClassInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'instVar1' to: 'instVar2' in: self class class.
	self assert: change changeClassName equals: self class name.
	self assert: change oldName equals: 'instVar1'.
	self assert: change newName equals: 'instVar2'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRenameClassVariable [
	| change |
	change := changes renameClassVariable: 'ClassVar1' to: 'ClassVar2' in: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change oldName equals: 'ClassVar1'.
	self assert: change newName equals: 'ClassVar2'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTest >> testRenameInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'instVar1' to: 'instVar2' in: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change oldName equals: 'instVar1'.
	self assert: change newName equals: 'instVar2'.
	self universalTestFor: change
]

{ #category : #mocking }
RBRefactoringChangeTest >> text [
	"for #testPerformAddRemoveMethodInteractively"
	^'method ^1'
]

{ #category : #utilities }
RBRefactoringChangeTest >> undoTestFor: aChange [
	| undo |
	undo := aChange asUndoOperation.
	self assert: (undo isKindOf: RBRefactoryChange)
]

{ #category : #utilities }
RBRefactoringChangeTest >> universalTestFor: aChange [
	self equalityTestFor: aChange.
	self stringTestFor: aChange.
	(aChange isKindOf: RBRefactoryClassChange)
		ifTrue: [ self undoTestFor: aChange ]
]
