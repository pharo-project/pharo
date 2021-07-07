Class {
	#name : #RBAddClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
RBAddClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddClassRefactoring };
		addCase: { #rbClass -> RBAddClassTransformation };
		yourself
]

{ #category : #parameterization }
RBAddClassParametrizedTest >> addClass [
	^ rbClass
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddClassParametrizedTest >> testAddClass [
	| refactoring newClass superClass classTest |
	
	refactoring := self addClass 
		addClass: #FooTest
		superclass: #RBAbstractRefactoringTest
		subclasses: (Array with: self class)
		category: #'Refactory-Testing'.
	self executeRefactoring: refactoring asRefactoring.
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
	self shouldFail: (self addClass
		addClass: #FooTest
		superclass: #RBRefactoringTest
		subclasses: (Array with: self class)
		category: #'') asRefactoring
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testExistingClassName [
	self shouldFail: (self addClass
				addClass: #Object
				superclass: self class name asSymbol
				subclasses: #()
				category: #'Refactory-Testing') asRefactoring
]

{ #category : #tests }
RBAddClassParametrizedTest >> testModelAddClass [
	| refactoring newClass superClass subclass |
	subclass := model classNamed: #Bar.
	superClass := model classNamed: #Foo.
	refactoring := self addClass 
		model: model
		addClass: #FooTest
		superclass: superClass name
		subclasses: (Array with: subclass)
		category: #'Refactory-Testing'.
	self executeRefactoring: refactoring asRefactoring.
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

	self shouldFail: (self addClass
				model: model
				addClass: #Foo
				superclass: #Object
				subclasses: #()
				category: #'Refactory-Testing') asRefactoring
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testModelNonImmediateSubclassOfSuperclass [
	| refactoring |
	refactoring := self addClass 
				model: model
				addClass: #Foo2
				superclass: #Object
				subclasses: (Array with: (model classNamed: #Bar))
				category: #'Refactory-Tesing'.
	self shouldFail: refactoring asRefactoring
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testNonImmediateSubclassOfSuperclass [
	self shouldFail: (self addClass 
			addClass: #Foo
			superclass: #RBCompositeLintRuleTestData
			subclasses: (Array with: RBBasicLintRuleTestData)
			category: #'Refactory-Tesing') asRefactoring
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testNonSymbolCategory [
	self shouldFail: (self addClass 
		addClass: #FooTest
		superclass: #RBRefactoringTest
		subclasses: (Array with: self class)
		category: 'Refactory-Testing') asRefactoring
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testSubclassIsMetaslass [
	self shouldFail: (self addClass 
			addClass: #Foo
			superclass: #RBLintRuleTestData
			subclasses: (Array with: RBCompositeLintRuleTestData class)
			category: #'Refactory-Tesing') asRefactoring
]

{ #category : #'failure tests' }
RBAddClassParametrizedTest >> testSuperclassIsMetaclass [
	self shouldFail: (self addClass 
				addClass: #Foo
				superclass: self class class name asSymbol
				subclasses: #()
				category: #'Refactory-Testing') asRefactoring
]
