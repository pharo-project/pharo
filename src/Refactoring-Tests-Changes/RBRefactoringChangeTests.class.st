Class {
	#name : #RBRefactoringChangeTests,
	#superclass : #TestCase,
	#instVars : [
		'changes'
	],
	#category : #Refactoring-Tests-Changes
}

{ #category : #accessing }
RBRefactoringChangeTests class >> packageNamesUnderTest [
	^ #('Refactoring-Changes')
]

{ #category : #mocking }
RBRefactoringChangeTests >> changeMock [
	^ Smalltalk at: #RBRefactoringChangeMock
]

{ #category : #mocking }
RBRefactoringChangeTests >> createMockClass [
	self class compiler evaluate: 'Object subclass: #RBRefactoringChangeMock
	instanceVariableNames: ''instVar''
	classVariableNames: ''ClassVar''
	poolDictionaries: ''''
	category: ''Refactoring-Tests-Changes'''.
	
	self class compiler evaluate:  'RBRefactoringChangeMock class
	instanceVariableNames: ''classInstVar'''.
	
	self changeMock compile: 'one ^ 1' classified: 'accessing'.
]

{ #category : #utilities }
RBRefactoringChangeTests >> equalityTestFor: aChange [
	self assert: aChange = aChange.
	self assert: aChange hash = aChange hash.

	self assert: aChange copy = aChange.
	self assert: aChange copy hash = aChange hash
]

{ #category : #accessing }
RBRefactoringChangeTests >> exampleClasses [
	^ { "Standard Classes" 
		ProtoObject. Object. Class. Metaclass. Behavior. ClassDescription. Dictionary. Trait.
		"Lots of class vars"
		SmalltalkImage
		""
	}
]

{ #category : #accessing }
RBRefactoringChangeTests >> exampleTraits [
	^ { TBehavior. TClass. TAssertable }
]

{ #category : #utilities }
RBRefactoringChangeTests >> perform: aChange do: aBlock [
	"Perform a change in the system silently, evaluate aBlock and then undo the change again."

	| undo |
	^ SystemAnnouncer uniqueInstance suspendAllWhile: [
		undo := aChange execute.
		aBlock ensure: [ undo execute ] ]
]

{ #category : #mocking }
RBRefactoringChangeTests >> removeMockClass [
	Smalltalk removeClassNamed: #RBRefactoringChangeMock.
]

{ #category : #mocking }
RBRefactoringChangeTests >> selectionInterval [
	^ 1 to: 0
]

{ #category : #running }
RBRefactoringChangeTests >> setUp [
	super setUp.
	self createMockClass.
	changes := RBRefactoryChangeManager changeFactory compositeRefactoryChangeNamed: 'testing'
]

{ #category : #utilities }
RBRefactoringChangeTests >> stringTestFor: aChange [
	self assert: (aChange name isString and: [ aChange name notEmpty ]).
	self assert: (aChange printString isString and: [ aChange printString notEmpty ]).
	self assert: (aChange changeString isString and: [ aChange changeString notEmpty ]).
	self assert: (aChange displayString isString and: [ aChange displayString notEmpty ])
]

{ #category : #running }
RBRefactoringChangeTests >> tearDown [
	super tearDown.
	self removeMockClass.
	
]

{ #category : #tests }
RBRefactoringChangeTests >> testAddClassInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'instVar' to: self class class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class class.
	self assert: change isMeta.
	self assert: change variable = 'instVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testAddClassInteractively [
	| change |
	change := RBRefactoryChangeManager changeFactory addClassDefinition: 'TestCase subclass: #' , self class name , '
	instanceVariableNames: ''instVar''
	classVariableNames: ''ClassVar''
	poolDictionaries: ''PoolDict''
	category: ''' , self class category , '''' for: self.
	self assert: change controller = self.
	self assert: change superclassName = self class superclass name.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change instanceVariableNames size = 1.
	self assert: change instanceVariableNames first = 'instVar'.
	self assert: change classVariableNames size = 1.
	self assert: change classVariableNames first = 'ClassVar'.
	self assert: change sharedPoolNames size = 1.
	self assert: change sharedPoolNames first = 'PoolDict'.
	self assert: change category = self class category.
	self universalTestFor: change
]

{ #category : #tests-pattern }
RBRefactoringChangeTests >> testAddClassPattern [
	"Make sure that all class definitions can be parsed."

	self exampleClasses do: [ :class |
		(class isObsolete or: [ class superclass notNil and: [ class superclass isObsolete ] ]) ifFalse: [
			| change |
			change := changes defineClass: class definition.
			self assert: (change isKindOf: RBAddClassChange).
			self assert: (change changeClassName = class name).
			self assert: (class superclass isNil
				ifTrue: [ change definitionClass = ProtoObject ]
				ifFalse: [ change definitionClass = class superclass ]).
			self assert: (change instanceVariableNames = class instVarNames asArray).
			self assert: (change classVariableNames = class classVarNames asArray).
			self assert: (change sharedPoolNames = class sharedPoolNames asArray).
			self assert: (change category = class category).
			self universalTestFor: change ] ]
]

{ #category : #tests-pattern }
RBRefactoringChangeTests >> testAddClassTraitPattern [
	"Make sure that all class trait definitions can be parsed."
	
	self exampleTraits do: [ :trait |
		| change |
		change := changes defineClass: trait classTrait definition.
		self assert: (change isKindOf: RBAddClassTraitChange).
		self assert: (change changeClassName = trait name).
		self universalTestFor: change ]
]

{ #category : #tests }
RBRefactoringChangeTests >> testAddClassVariable [
	| change |
	change := changes addClassVariable: 'ClassVar' to: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'ClassVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testAddInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'instVar' to: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'instVar'.
	self universalTestFor: change
]

{ #category : #tests-pattern }
RBRefactoringChangeTests >> testAddMetaclassPattern [
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
RBRefactoringChangeTests >> testAddPool [
	| change |
	change := changes addPool: 'PoolDict' to: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'PoolDict'.
	self universalTestFor: change
]

{ #category : #tests-pattern }
RBRefactoringChangeTests >> testAddTraitPattern [
	"Make sure that all trait definitions can be parsed."
	
	self exampleTraits do: [ :trait |
		| change |
		change := changes defineClass: trait definition.
		self assert: (change isKindOf: RBAddTraitChange).
		self assert: (change changeClassName = trait name).
		self assert: (change category = trait category).
		self universalTestFor: change ]
]

{ #category : #tests }
RBRefactoringChangeTests >> testComment [
	| change |
	change := changes comment: 'Some Comment' in: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change comment = 'Some Comment'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testCompileInClass [
	| change |
	change := changes compile: 'setUp' in: self class.
	self assert: change controller isNil.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self assert: change source = 'setUp'.
	self assert: change protocol = #running.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testCompileInClassified [
	| change |
	change := changes compile: 'setUp' in: self class classified: #accessing.
	self assert: change controller isNil.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self assert: change source = 'setUp'.
	self assert: change protocol = #accessing.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testCompileInInteractively [
	| change |
	change := RBRefactoryChangeManager changeFactory addMethodSource: 'setUp' in: self class classified: #running for: self.
	self assert: change controller = self.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self assert: change source = 'setUp'.
	self assert: change protocol = #running.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testCompileInMetaclass [
	| change |
	change := changes compile: 'new' in: self class class.
	self assert: change controller isNil.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class class.
	self assert: change isMeta.
	self assert: change selector = #new.
	self assert: change source = 'new'.
	self assert: change protocol = #accessing.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testComposite [
	changes 
		compile: 'method ^ 1' in: self class classified: #utilities;
		compile: 'method ^ 2' in: self class class classified: #utilities.
	self assert: (changes changesSize = 2).
	self assert: (changes problemCount = 2).
	self assert: (changes changeForClass: self class name selector: #method) notNil.
	self assert: (changes changeForMetaclass: self class name selector: #method) notNil.
	self universalTestFor: changes
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveClass [
	| change |
	change := changes defineClass: 'Object subclass: #' , self changeMock name , 'Temporary
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , self class category , ''''.
	self perform: change do: [
		self assert: (Smalltalk hasClassNamed: change changeClassName).
		self assert: change definedClass name =  change changeClassName.
		self assert: change definedClass isBehavior ].
	self deny: (Smalltalk hasClassNamed: change changeClassName).
	self assert: change definedClass isObsolete
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveClassInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'foo' to: self changeMock class.
	self perform: change do: [ self assert: (change changeClass instVarNames includes: 'foo') ].
	self deny: (change changeClass instVarNames includes: 'foo')
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveClassInteractively [
	| change |
	change := RBRefactoryChangeManager changeFactory addClassDefinition: 'Object subclass: #' , self changeMock name , 'Temporary
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , self class category , '''' for: self.
	self perform: change do: [ 
		self assert: (Smalltalk hasClassNamed: change changeClassName).
		self assert: change definedClass name =  change changeClassName.
		self assert: change definedClass isBehavior ].
	self deny: (Smalltalk hasClassNamed: change changeClassName).
	self assert: change definedClass isObsolete
	
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveClassMethod [
	| change |
	change := changes compile: 'method ^ 1' in: self changeMock class classified: #utilities.
	self perform: change do: [ self assert: (self changeMock respondsTo: #method) ].
	self deny: (self changeMock respondsTo: #method).
	self assert: change definedSelector = #method
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveClassVariable [
	| change |
	change := changes addClassVariable: 'Foo' to: self changeMock.
	self perform: change do: [ self assert: (change changeClass classVarNames includes: 'Foo') ].
	self deny: (change changeClass classVarNames includes: 'Foo')
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveInstanceVariable [
	| change |
	change := changes addInstanceVariable: 'foo' to: self changeMock.
	self perform: change do: [ self assert: (change changeClass instVarNames includes: 'foo') ].
	self deny: (change changeClass instVarNames includes: 'foo')
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveMethod [
	| change |
	change := changes compile: 'method ^ 1' in: self changeMock classified: #utilities.
	self perform: change do: [ self assert: (self changeMock canUnderstand: #method) ].
	self deny: (self changeMock canUnderstand: #method).
	self assert: change definedSelector = #method
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformAddRemoveMethodInteractively [
	| change |
	change := RBRefactoryChangeManager changeFactory addMethodSource: 'method ^ 1' in: self changeMock classified: #utilities for: self. 
	self perform: change do: [ self assert: (self changeMock canUnderstand: #method) ].
	self deny: (self changeMock canUnderstand: #method).
	self assert: change definedSelector = #method
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformChangeClass [
	| change |
	change := changes defineClass: self class name , ' subclass: #' , self changeMock name , '
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , self class category , ''''.
	self perform: change do: [ 
		self assert: change changeClass superclass = self class.
		self assert: change changeClass instVarNames isEmpty ].
	self assert: change changeClass superclass = Object.
	self assert: change changeClass instVarNames notEmpty.
	self assert: change definedClass = self changeMock
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformChangeComment [
	| change comment |
	change := changes comment: 'Some Comment' in: self changeMock.
	comment := change changeClass organization classComment.
	self perform: change do: [ self assert: (change changeClass comment = 'Some Comment') ].
	self assert: change changeClass organization classComment = comment
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformChangeMetaclass [
	| change |
	change := changes defineClass: self changeMock name , ' class 
	instanceVariableNames: '''''.
	self perform: change do: [ self assert: change changeClass class instVarNames isEmpty ].
	self assert: change changeClass class instVarNames notEmpty.
	self assert: change definedClass = self changeMock class
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformChangeMethod [
	| change source |
	change := changes compile: 'one ^ 2' in: self changeMock.
	source := change changeClass sourceCodeAt: #one.
	self perform: change do: [ self assert: (change changeClass sourceCodeAt: #one) = 'one ^ 2' ].
	self assert: (change changeClass sourceCodeAt: #one) = source
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformCompositeChange [
	changes 
		compile: 'method1 ^ 1' in: self changeMock;
		compile: 'method2 ^ 2' in: self changeMock.
	self perform: changes do: [ 
		self assert: (self changeMock canUnderstand: #method1).
		self assert: (self changeMock canUnderstand: #method2) ].
	self deny: (self changeMock canUnderstand: #method1).
	self deny: (self changeMock canUnderstand: #method2) 
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformRenameClass [
	| change |
	change := changes renameClass: self changeMock to: self changeMock name , 'Plus'.
	self perform: change do: [ 
		self deny: (Smalltalk hasClassNamed: change oldName).
		self assert: (Smalltalk hasClassNamed: change newName) ].
	self assert: (Smalltalk hasClassNamed: change oldName).
	self deny: (Smalltalk hasClassNamed: change newName)
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformRenameClassInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'classInstVar' to: 'classInstVarPlus' in: self changeMock class.
	self perform: change do: [ 
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformRenameClassVariable [
	| change |
	change := changes renameClassVariable: 'ClassVar' to: 'ClassVarPlus' in: self changeMock.
	self perform: change do: [ 
		self deny: (change changeClass classVarNames includes: change oldName).
		self assert: (change changeClass classVarNames includes: change newName) ].
	self assert: (change changeClass classVarNames includes: change oldName).
	self deny: (change changeClass classVarNames includes: change newName)
]

{ #category : #tests-perform }
RBRefactoringChangeTests >> testPerformRenameInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'instVar' to: 'instVarPlus' in: self changeMock.
	self perform: change do: [ 
		self deny: (change changeClass instVarNames includes: change oldName).
		self assert: (change changeClass instVarNames includes: change newName) ].
	self assert: (change changeClass instVarNames includes: change oldName).
	self deny: (change changeClass instVarNames includes: change newName)
]

{ #category : #tests }
RBRefactoringChangeTests >> testRemoveClass [
	| change |
	change := changes removeClass: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRemoveClassInstanceVariable [
	| change |
	change := changes removeInstanceVariable: 'instVar' from: self class class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class class.
	self assert: change isMeta.
	self assert: change variable = 'instVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRemoveClassNamed [
	| change |
	change := changes removeClassNamed: self class name.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRemoveClassVariable [
	| change |
	change := changes removeClassVariable: 'ClassVar' from: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'ClassVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRemoveInstanceVariable [
	| change |
	change := changes removeInstanceVariable: 'instVar' from: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'instVar'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRemoveMethod [
	| change |
	change := changes removeMethod: #setUp from: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change selector = #setUp.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRemovePool [
	| change |
	change := changes removePool: 'PoolDict' from: self class.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change variable = 'PoolDict'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRenameClass [
	| change |
	change := changes renameClass: self class to: self class name , 'Plus'.
	self assert: change oldName = self class name.
	self assert: change newName = (self class name , 'Plus').
	self assert: change changeClass = self class.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRenameClassInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'instVar1' to: 'instVar2' in: self class class.
	self assert: change changeClassName = self class name.
	self assert: change oldName = 'instVar1'.
	self assert: change newName = 'instVar2'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRenameClassVariable [
	| change |
	change := changes renameClassVariable: 'ClassVar1' to: 'ClassVar2' in: self class.
	self assert: change changeClassName = self class name.
	self assert: change oldName = 'ClassVar1'.
	self assert: change newName = 'ClassVar2'.
	self universalTestFor: change
]

{ #category : #tests }
RBRefactoringChangeTests >> testRenameInstanceVariable [
	| change |
	change := changes renameInstanceVariable: 'instVar1' to: 'instVar2' in: self class.
	self assert: change changeClassName = self class name.
	self assert: change oldName = 'instVar1'.
	self assert: change newName = 'instVar2'.
	self universalTestFor: change
]

{ #category : #mocking }
RBRefactoringChangeTests >> text [
	"for #testPerformAddRemoveMethodInteractively"
	^'method ^1'
]

{ #category : #utilities }
RBRefactoringChangeTests >> undoTestFor: aChange [
	| undo |
	undo := aChange asUndoOperation.
	self assert: (undo isKindOf: RBRefactoryChange)
]

{ #category : #utilities }
RBRefactoringChangeTests >> universalTestFor: aChange [
	self equalityTestFor: aChange.
	self stringTestFor: aChange.
	(aChange isKindOf: RBRefactoryClassChange)
		ifTrue: [ self undoTestFor: aChange ]
]
