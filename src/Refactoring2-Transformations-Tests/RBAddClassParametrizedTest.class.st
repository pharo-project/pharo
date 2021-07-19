Class {
	#name : #RBAddClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddClassRefactoring };
		addCase: { #rbClass -> RBAddClassTransformation };
		yourself
]

{ #category : #accessing }
RBAddClassParametrizedTest >> constructor [ 
	^ #addClass:superclass:subclasses:category: 
]

{ #category : #running }
RBAddClassParametrizedTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddClassParametrizedTest >> testAddClass [
	| refactoring newClass superClass classTest |
	
	refactoring := self createRefactoringWithArguments: 
		{ #FooTest . #RBAbstractRefactoringTest . (Array with: self class) . #'Refactory-Testing' }.
		
	self executeRefactoring: refactoring.
	
	newClass := refactoring model classNamed: #FooTest.
	superClass := refactoring model classNamed: #RBAbstractRefactoringTest.
	classTest := refactoring model classNamed: self class name.
	self assert: newClass superclass equals: superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass classSide superclass equals: superClass classSide.
	self assert: (superClass classSide subclasses includes: newClass classSide).
	self assert: classTest superclass equals: newClass.
	self assert: (newClass subclasses includes: classTest).
	self assert: classTest classSide superclass equals: newClass classSide.
	self assert: (newClass classSide subclasses includes: classTest classSide)
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testEmptyCategory [
	self shouldFail: (self createRefactoringWithArguments:
		{ #FooTest . #RBAbstractRefactoringTest . (Array with: self class) . #'' })
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testExistingClassName [
	self shouldFail: (self createRefactoringWithArguments:
		{ #Object . self class name asSymbol . #() .  #'Refactory-Testing' } )
]

{ #category : #tests }
RBAddClassParametrizedTest >> testModelAddClass [
	| refactoring newClass superClass subclass |
	subclass := model classNamed: #Bar.
	superClass := model classNamed: #Foo.
	refactoring := self createRefactoringWithModel: model andArguments:
		{ #FooTest . superClass name . (Array with: subclass) . #'Refactory-Testing' }.
		
	self executeRefactoring: refactoring.
	
	newClass := model classNamed: #FooTest.
	self assert: newClass superclass equals: superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass classSide superclass equals: superClass classSide.
	self assert: (superClass classSide subclasses includes: newClass classSide).
	self assert: subclass superclass equals: newClass.
	self assert: (newClass subclasses includes: subclass).
	self assert: subclass classSide superclass equals: newClass classSide.
	self assert: (newClass classSide subclasses includes: subclass classSide)
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testModelExistingClassName [

	self shouldFail: (self createRefactoringWithModel: model andArguments: 
		{ #Foo . #Object . #() . #'Refactory-Testing' })
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testModelInvalidSubclass [

	self shouldFail: (self createRefactoringWithModel: model andArguments: 
		{ #Foo2 . #Object . (Array with: (model classNamed: #Bar)) . #'Refactory-Tesing' } )
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testModelNonImmediateSubclassOfSuperclass [
	| refactoring |
	refactoring := self createRefactoringWithModel: model andArguments:  
		{ #Foo2 . #Object .(Array with: (model classNamed: #Bar)) . #'Refactory-Tesing'}.
		
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testNonImmediateSubclassOfSuperclass [
	self shouldFail: (self createRefactoringWithArguments: 
		{ #Foo . #RBCompositeLintRuleTestData . (Array with: RBBasicLintRuleTestData) . #'Refactory-Tesing'})
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testSubclassIsMetaslass [
	self shouldFail: (self createRefactoringWithArguments: 
		{ #Foo . #RBLintRuleTestData . (Array with: RBCompositeLintRuleTestData class) . #'Refactory-Tesing'})
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testSuperclassIsMetaclass [
	self shouldFail: (self createRefactoringWithArguments:  
		{ #Foo . self class class name asSymbol . #() . #'Refactory-Testing'})
]
