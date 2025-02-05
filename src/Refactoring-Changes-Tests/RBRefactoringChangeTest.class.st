Class {
	#name : 'RBRefactoringChangeTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'changes',
		'workingEnvironment',
		'changeFactory'
	],
	#category : 'Refactoring-Changes-Tests',
	#package : 'Refactoring-Changes-Tests'
}

{ #category : 'accessing' }
RBRefactoringChangeTest class >> packageNamesUnderTest [
	^ #('Refactoring-Changes')
]

{ #category : 'mocking' }
RBRefactoringChangeTest >> changeMockClass [
	^ RBRefactoringChangeMock
]

{ #category : 'utilities' }
RBRefactoringChangeTest >> equalityTestFor: aChange [
	self assert: aChange equals: aChange.
	self assert: aChange hash equals: aChange hash.

	self assert: aChange copy equals: aChange.
	self assert: aChange copy hash equals: aChange hash
]

{ #category : 'accessing' }
RBRefactoringChangeTest >> exampleClasses [
	<sampleInstance>
	^ { "Standard Classes"
		ProtoObject. Object. Class. Metaclass. Behavior. ClassDescription. Dictionary. Trait.

		"Lots of class vars"
		SmalltalkImage
	}
]

{ #category : 'accessing' }
RBRefactoringChangeTest >> exampleTraits [
    <sampleInstance>
	^ { TSortable. TAssertable }
]

{ #category : 'utilities' }
RBRefactoringChangeTest >> performAndUndoChange: aChange do: aBlock [
	"Perform a change in the system silently, evaluate aBlock and then undo the change again."

	^ self class codeChangeAnnouncer suspendAllWhile: [
		  | undo |
		  undo := aChange execute.
		  aBlock ensure: [ undo execute ] ]
]

{ #category : 'mocking' }
RBRefactoringChangeTest >> selectionInterval [
	^ 1 to: 0
]

{ #category : 'running' }
RBRefactoringChangeTest >> setUp [
	super setUp.
	workingEnvironment := Smalltalk globals.
	"In the future we should make sure that the tests can run on a new environment."
	changeFactory := RBRefactoryChangeManager changeFactory.
	"changeFactory is used in tests too"
	changes := changeFactory compositeRefactoryChangeNamed: 'testing'
]

{ #category : 'utilities' }
RBRefactoringChangeTest >> stringTestFor: aChange [
	self assert: (aChange name isString and: [ aChange name notEmpty ]).
	self assert: (aChange printString isString and: [ aChange printString notEmpty ]).
	self assert: (aChange changeString isString and: [ aChange changeString notEmpty ]).
	self assert: (aChange displayString isString and: [ aChange displayString notEmpty ])
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testAddClassInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'instVar' to: self class class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class class.
	self assert: change isMeta.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testAddClassInteractively [

	| change |
	change := changeFactory addClassDefinition: [ :aBuilder |
		          aBuilder
			          superclass: AbstractEnvironmentTestCase;
			          name: self class name;
			          slots: { #instVar };
			          sharedVariables: { #ClassVar };
			          sharedPools: #(PoolDict);
			          tag: self class packageTag name;
			          package: self class package name ].
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
	self assert: change package equals: self class package name.
	self assert: change tag equals: self class packageTag name.
	self universalTestFor: change
]

{ #category : 'tests - pattern' }
RBRefactoringChangeTest >> testAddClassPattern [
	"Make sure that all class definitions can be parsed."

	self exampleClasses do: [ :class |
		(class isObsolete or: [ class superclass isNotNil and: [ class superclass isObsolete ] ]) ifFalse: [
			| change |
			change := changes defineClass: [ :aBuilder | aBuilder fillFor: class ].
			self assert: (change isKindOf: RBAddClassChange).
			self assert: change changeClassName equals: class name.

			class superclass
				ifNil: [ self assert: change definitionClass equals: ProtoObject ]
				ifNotNil: [ self assert: change definitionClass equals: class superclass ].

			self assert: change instanceVariableNames equals: class instVarNames asArray.
			self assert: change classVariableNames asSet equals: class classVarNames asSet.
			self assert: change classInstanceVariableNames equals: class class instVarNames.
			self assert: change sharedPoolNames equals: class sharedPoolNames asArray.
			self assert: change package equals: class package name.
			self assert: change tag equals: class packageTag name.
			self universalTestFor: change ] ]
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testAddClassVariable [

	| change |
	change := changes addClassVariable: 'SharedVarTheta' to: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'SharedVarTheta'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testAddInstanceVariable [

	| change |
	change := changes addInstanceVariable: 'instVar' to: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testAddPool [

	| change |
	change := changes addPool: 'SharedPool' to: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'SharedPool'.
	self universalTestFor: change
]

{ #category : 'tests - pattern' }
RBRefactoringChangeTest >> testAddTraitPattern [
	"Make sure that all trait definitions can be parsed."

	self exampleTraits do: [ :trait |
		| change |
		change := changes defineClass: [ :aBuilder | aBuilder fillFor: trait ].
		self assert: change changeClassName equals: trait name.
		self assert: change package equals: trait package name.
		self assert: change tag equals: (trait packageTag isRoot
				 ifTrue: [ nil ]
				 ifFalse: [ trait packageTag name ]).
		self universalTestFor: change ]
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testComment [

	| change |
	change := changes comment: 'Some Comment' in: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change comment equals: 'Some Comment'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testCompileInClass [
	"note that this compile does not modify the class because the change is not performed"
	| change |
	change := changes compile: 'setUp' in: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self assert: change source equals: 'setUp'.
	self assert: change protocol equals: #running.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testCompileInClassified [
	"note that this compile does not modify the class because the change is not performed"
	| change |
	change := changes compile: 'setUp' in: self class classified: #accessing.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self assert: change source equals: 'setUp'.
	self assert: change protocol equals: #accessing.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testCompileInInteractively [
	"note that this compile does not modify the class because the change is not performed"
	
	| change |
	change := changeFactory addMethodSource: 'setUp' in: self class classified: #running.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self assert: change source equals: 'setUp'.
	self assert: change protocol equals: #running.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testCompileInMetaclass [
	"note that this compile does not modify the class because the change is not performed"
	
	| change |
	change := changes compile: 'new' in: self class class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class class.
	self assert: change isMeta.
	self assert: change selector equals: #new.
	self assert: change source equals: 'new'.
	self assert: change protocol equals: Protocol unclassified.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testComposite [
	"note that this compile does not modify the class because the change is not performed"
	
	changes
		compile: 'method ^ 1' in: self class classified: #utilities;
		compile: 'method ^ 2' in: self class class classified: #utilities.
	self assert: changes changesSize equals: 2.
	self assert: changes problemCount equals: 2.
	self assert: (changes changeForClass: self class name selector: #method) isNotNil.
	self assert: (changes changeForMetaclass: self class name selector: #method) isNotNil.
	self universalTestFor: changes
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClass [

	| change |
	change := changes defineClass: [ :aBuilder |
		          aBuilder
			          superclass: Object;
			          name: self changeMockClass name , 'Temporary';
			          package: self class package name ].
	self performAndUndoChange: change do: [
		self assert: (workingEnvironment hasClassNamed: change changeClassName).
		self assert: change definedClass name equals: change changeClassName.
		self assert: change definedClass isBehavior ].
	self deny: (workingEnvironment hasClassNamed: change changeClassName).
	self assert: change definedClass isObsolete
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassInstanceVariable [

	| change |
	change := changes addInstanceVariable: 'foo' to: self changeMockClass class.
	self performAndUndoChange: change do: [ self assert: (change changeClass instVarNames includes: 'foo') ].
	self deny: (change changeClass instVarNames includes: 'foo')
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassInteractively [

	| change |
	change := changeFactory addClassDefinition: [ :aBuilder |
		          aBuilder
			          superclass: Object;
			          name: self changeMockClass name , 'Temporary';
			          package: self class package name ].
	self performAndUndoChange: change do: [
		self assert: (workingEnvironment hasClassNamed: change changeClassName).
		self assert: change definedClass name equals: change changeClassName.
		self assert: change definedClass isBehavior ].
	self deny: (workingEnvironment hasClassNamed: change changeClassName).
	self assert: change definedClass isObsolete
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassMethod [

	| change |
	change := changes compile: 'method ^ 1' in: self changeMockClass class classified: #utilities.
	self performAndUndoChange: change do: [ self assert: (self changeMockClass respondsTo: #method) ].
	self deny: (self changeMockClass respondsTo: #method).
	self assert: change definedSelector equals: #method
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveClassVariable [

	| change |
	change := changes addClassVariable: 'Foo' to: self changeMockClass.
	self performAndUndoChange: change do: [ self assert: (change changeClass classVarNames includes: 'Foo') ].
	self deny: (change changeClass classVarNames includes: 'Foo')
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveInstanceVariable [

	| change |
	change := changes addInstanceVariable: 'foo' to: self changeMockClass.
	self performAndUndoChange: change do: [ self assert: (change changeClass instVarNames includes: 'foo') ].
	self deny: (change changeClass instVarNames includes: 'foo')
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveMethod [
	| change |
	change := changes compile: 'method ^ 1' in: self changeMockClass classified: #utilities.
	self performAndUndoChange: change do: [ self assert: (self changeMockClass canUnderstand: #method) ].
	self deny: (self changeMockClass canUnderstand: #method).
	self assert: change definedSelector equals: #method
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformAddRemoveMethodInteractively [

	| change |
	change := changeFactory addMethodSource: 'method ^ 1' in: self changeMockClass classified: #utilities.
	self performAndUndoChange: change do: [ self assert: (self changeMockClass canUnderstand: #method) ].
	self deny: (self changeMockClass canUnderstand: #method).
	self assert: change definedSelector equals: #method
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformChangeClass [

	| change |
	change := changes defineClass: [ :aBuilder |
		          aBuilder
			          superclass: self class;
			          name: self changeMockClass name;
			          package: self class package name ].
	self performAndUndoChange: change do: [
		self assert: change changeClass superclass equals: self class.
		self assertEmpty: change changeClass instVarNames.
		self assertEmpty: change changeClass classVarNames ].
	self assert: change changeClass superclass equals: RBRefactoryChange.
	self denyEmpty: change changeClass instVarNames.
	self denyEmpty: change changeClass classVarNames.
	self assert: change definedClass equals: self changeMockClass
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformChangeClassInstanceVariables [

	| change |
	change := changes defineClass: [ :aBuilder |
		          aBuilder
			          fillFor: self changeMockClass;
			          classSlots: {  } ].
	self performAndUndoChange: change do: [ self assertEmpty: change changeClass class instVarNames ].
	self denyEmpty: change changeClass class instVarNames.
	self assert: change definedClass equals: self changeMockClass
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformChangeComment [

	| change comment |
	change := changes comment: 'Some Comment' in: self changeMockClass.
	comment := change changeClass comment.
	self performAndUndoChange: change do: [ self assert: change changeClass comment equals: 'Some Comment' ].
	self assert: change changeClass comment equals: comment
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformChangeMethod [

	| change source |
	change := changes compile: 'one ^ 2' in: self changeMockClass.
	source := change changeClass sourceCodeAt: #one.
	self performAndUndoChange: change do: [ self assert: (change changeClass sourceCodeAt: #one) equals: 'one ^ 2' ].
	self assert: (RBRefactoringChangeMock >> #one) sourceCode equals: 'one

	^ 1'.
	self assert: (change changeClass sourceCodeAt: #one) equals: source
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformCompositeChange [
	changes
		compile: 'method1 ^ 1' in: self changeMockClass;
		compile: 'method2 ^ 2' in: self changeMockClass.
	self performAndUndoChange: changes do: [
		self assert: (self changeMockClass canUnderstand: #method1).
		self assert: (self changeMockClass canUnderstand: #method2) ].
	self deny: (self changeMockClass canUnderstand: #method1).
	self deny: (self changeMockClass canUnderstand: #method2)
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformRenameClass [

	| change |
	change := changes 
		renameClass: self changeMockClass 
		to: self changeMockClass name , 'Plus'.
	self performAndUndoChange: change do: [
		self deny: (workingEnvironment hasClassNamed: change oldName).
		self assert: (workingEnvironment hasClassNamed: change newName) ].
	self assert: (workingEnvironment hasClassNamed: change oldName).
	self deny: (workingEnvironment hasClassNamed: change newName)
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformRenameClassInstanceVariable [

	| change |
	change := changes renameInstanceVariable: 'classInstVar' 
		to: 'classInstVarPlus' in: self changeMockClass class.
	self performAndUndoChange: change do: [
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformRenameClassVariable [
	
	| change |
	change := changes 
		renameClassVariable: 'SharedVar' 
		to: 'SharedVarPlus' 
		in: self changeMockClass.
	self performAndUndoChange: change do: [
		self deny: (change changeClass classVarNames includes: change oldName).
		self assert: (change changeClass classVarNames includes: change newName) ].
	self assert: (change changeClass classVarNames includes: change oldName).
	self deny: (change changeClass classVarNames includes: change newName)
]

{ #category : 'tests - perform' }
RBRefactoringChangeTest >> testPerformRenameInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'instVar' to: 'instVarPlus' in: self changeMockClass.
	self performAndUndoChange: change do: [
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRemoveClass [

	| change |
	change := changes removeClass: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRemoveClassInstanceVariable [

	| change |
	change := changes removeInstanceVariable: 'instVar' from: self class class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class class.
	self assert: change isMeta.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRemoveClassNamed [

	| change |
	change := changes removeClassNamed: self class name.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRemoveClassVariable [

	| change |
	change := changes removeClassVariable: 'SharedVar' from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'SharedVar'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRemoveInstanceVariable [

	| change |
	change := changes removeInstanceVariable: 'instVar' from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'instVar'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRemoveMethod [

	| change |
	change := changes removeMethod: #setUp from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change selector equals: #setUp.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRemovePool [

	| change |
	change := changes removePool: 'SharedPool' from: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change changeClass equals: self class.
	self assert: change isMeta not.
	self assert: change variable equals: 'SharedPool'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRenameClass [

	| change |
	change := changes renameClass: self class to: self class name , 'Plus'.
	self assert: change oldName equals: self class name.
	self assert: change newName equals: (self class name , 'Plus').
	self assert: change changeClass equals: self class.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRenameClassInstanceVariable [

	| change |
	change := changes renameInstanceVariable: 'instVar1' to: 'instVar2' in: self class class.
	self assert: change changeClassName equals: self class name.
	self assert: change oldName equals: 'instVar1'.
	self assert: change newName equals: 'instVar2'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRenameClassVariable [

	| change |
	change := changes renameClassVariable: 'SharedVar1' to: 'SharedVar2' in: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change oldName equals: 'SharedVar1'.
	self assert: change newName equals: 'SharedVar2'.
	self universalTestFor: change
]

{ #category : 'tests' }
RBRefactoringChangeTest >> testRenameInstanceVariable [

	| change |
	change := changes renameInstanceVariable: 'instVar1' to: 'instVar2' in: self class.
	self assert: change changeClassName equals: self class name.
	self assert: change oldName equals: 'instVar1'.
	self assert: change newName equals: 'instVar2'.
	self universalTestFor: change
]

{ #category : 'mocking' }
RBRefactoringChangeTest >> text [
	"for #testPerformAddRemoveMethodInteractively"
	^'method ^1'
]

{ #category : 'utilities' }
RBRefactoringChangeTest >> undoTestFor: aChange [
	| undo |
	undo := aChange asUndoOperation.
	self assert: (undo isKindOf: RBRefactoryChange)
]

{ #category : 'utilities' }
RBRefactoringChangeTest >> universalTestFor: aChange [
	self equalityTestFor: aChange.
	self stringTestFor: aChange.
	(aChange isKindOf: RBRefactoryClassChange)
		ifTrue: [ self undoTestFor: aChange ]
]
