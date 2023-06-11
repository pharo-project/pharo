Class {
	#name : #RBInsertClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBInsertClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInsertNewClassRefactoring };
		addCase: { #rbClass -> RBInsertNewClassTransformation };
		yourself
]

{ #category : #accessing }
RBInsertClassParametrizedTest >> constructor [
	^ #addClass:superclass:subclasses:category:
]

{ #category : #running }
RBInsertClassParametrizedTest >> setUp [

	super setUp.
	model := self abstractVariableTestData
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureEmptyCategory [

	self shouldFail: (self createRefactoringWithArguments: {
				 #FooTest.
				 #RBAbstractRefactoringTest.
				 (Array with: self class).
				 #'' })
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureExistingClassName [

	self shouldFail: (self createRefactoringWithArguments: {
				 #Object.
				 self class name asSymbol.
				 #(  ).
				 #'Refactory-Testing' })
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureModelExistingClassName [

	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 #Foo.
				 #Object.
				 #(  ).
				 #'Refactory-Testing' })
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureModelInvalidSubclass [

	self shouldFail:
		(self createRefactoringWithModel: model andArguments: {
				 #Foo2.
				 #Object.
				 (Array with: (model classNamed: #Bar)).
				 #'Refactory-Tesing' })
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureModelNonImmediateSubclassOfSuperclass [

	| refactoring |
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               #Foo2.
				               #Object.
				               (Array with: (model classNamed: #Bar)).
				               #'Refactory-Tesing' }.

	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureNonImmediateSubclassOfSuperclass [

	self shouldFail: (self createRefactoringWithArguments: {
				 #Foo.
				 #RBCompositeLintRuleTestData.
				 (Array with: RBBasicLintRuleTestData).
				 #'Refactory-Tesing' })
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureSubclassIsMetaslass [

	self shouldFail: (self createRefactoringWithArguments: {
				 #Foo.
				 #RBLintRuleTestData.
				 (Array with: RBCompositeLintRuleTestData class).
				 #'Refactory-Tesing' })
]

{ #category : #'failure tests' }
RBInsertClassParametrizedTest >> testFailureSuperclassIsMetaclass [

	self shouldFail: (self createRefactoringWithArguments: {
				 #Foo.
				 self class class name asSymbol.
				 #(  ).
				 #'Refactory-Testing' })
]

{ #category : #tests }
RBInsertClassParametrizedTest >> testInsertClassWithinExistingHiearchy [
	| refactoring insertedClass parentOfInsertedClass childOfInsertedClass |

	refactoring := self createRefactoringWithArguments:
		{ #InsertedClass . #RBAbstractRefactoringTest . (Array with: self class) . #'Refactory-Testing' }.
		"addClass: superclass: subclasses: category:"
	self executeRefactoring: refactoring.

	insertedClass := refactoring model classNamed: #InsertedClass.
	parentOfInsertedClass := refactoring model classNamed: #RBAbstractRefactoringTest.
	childOfInsertedClass := refactoring model classNamed: self class name.

	"The inserted class is correctly inserted between the currrent class and its superclass.
	The checks only check the inserted-superclass link."
	self assert: insertedClass superclass equals: parentOfInsertedClass.
	self assert: (parentOfInsertedClass subclasses includes: insertedClass).

	"Metaclasses are correctly linked"
	self assert: insertedClass classSide superclass equals: parentOfInsertedClass classSide.
	self assert: (parentOfInsertedClass classSide subclasses includes: insertedClass classSide).

	"The inserted class is correctly inserted between the currrent class and its superclass.
	The checks only check the insertedclass and its subclasses."
	self assert: childOfInsertedClass superclass equals: insertedClass.
	self assert: (insertedClass subclasses includes: childOfInsertedClass).
	self assert: childOfInsertedClass classSide superclass equals: insertedClass classSide.
	self assert: (insertedClass classSide subclasses includes: childOfInsertedClass classSide)
]

{ #category : #tests }
RBInsertClassParametrizedTest >> testModelInsertClass [
	| refactoring insertedClass superClass subclass |
	superClass := model classNamed: #Foo.
	subclass := model classNamed: #Bar.

	refactoring := self createRefactoringWithModel: model andArguments:
		{ #InsertedClass . superClass name . (Array with: subclass) . #'Refactory-Testing' }.
		"addClass: superclass: subclasses: category:"

	self executeRefactoring: refactoring.

	insertedClass := model classNamed: #InsertedClass.
	self assert: insertedClass superclass equals: superClass.
	self assert: (superClass subclasses includes: insertedClass).

	self assert: insertedClass classSide superclass equals: superClass classSide.
	self assert: (superClass classSide subclasses includes: insertedClass classSide).

	self assert: subclass superclass equals: insertedClass.
	self assert: (insertedClass subclasses includes: subclass).
	self assert: subclass classSide superclass equals: insertedClass classSide.
	self assert: (insertedClass classSide subclasses includes: subclass classSide)
]
