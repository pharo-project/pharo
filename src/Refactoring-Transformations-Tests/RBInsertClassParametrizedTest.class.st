Class {
	#name : 'RBInsertClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBInsertClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBInsertNewClassRefactoring };
		addCase: { #rbClass -> RBInsertNewClassTransformation };
		yourself
]

{ #category : 'accessing' }
RBInsertClassParametrizedTest >> constructor [
	"There is not one specific constructor."

	^ self shouldNotImplement
]

{ #category : 'running' }
RBInsertClassParametrizedTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'failure tests' }
RBInsertClassParametrizedTest >> testFailureEmptyPackage [

	self shouldFail: ((rbClass className: #FooTest)
			 superclass: #RBAbstractRefactoringTest;
			 subclasses: { self class };
			 packageName: #'')
]

{ #category : 'failure tests' }
RBInsertClassParametrizedTest >> testFailureExistingClassName [

	self shouldFail: ((rbClass className: #Object)
			 superclass: self class name;
			 packageName: #'Refactoring-Testirg')
]

{ #category : 'failure tests' }
RBInsertClassParametrizedTest >> testFailureModelExistingClassName [

	self shouldFail: ((rbClass model: model className: #Foo)
			 superclass: #Object;
			 packageName: #'Refactoring-Testing')
]

{ #category : 'failure tests' }
RBInsertClassParametrizedTest >> testFailureModelInvalidSubclass [

	self shouldFail: ((rbClass model: model className: #Foo2)
			 superclass: #Object;
			 subclasses: { (model classNamed: #Bar) };
			 packageName: #'Refactoring-Testing')
]

{ #category : 'failure tests' }
RBInsertClassParametrizedTest >> testFailureNonImmediateSubclassOfSuperclass [

	self shouldFail: ((rbClass className: #Foo)
			 superclass: #RBCompositeLintRuleTestData;
			 subclasses: { RBBasicLintRuleTestData };
			 packageName: #'Refactoring-Testing')
]

{ #category : 'failure tests' }
RBInsertClassParametrizedTest >> testFailureSubclassIsMetaslass [

	self shouldFail: ((rbClass className: #Foo)
			 superclass: #RBLintRuleTestData;
			 subclasses: { RBCompositeLintRuleTestData class };
			 packageName: #'Refactoring-Testing')
]

{ #category : 'failure tests' }
RBInsertClassParametrizedTest >> testFailureSuperclassIsMetaclass [

	self shouldFail: ((rbClass className: #Foo)
			 superclass: self class class name;
			 packageName: #'Refactoring-Testing')
]

{ #category : 'tests' }
RBInsertClassParametrizedTest >> testInsertClassWithinExistingHiearchy [

	| refactoring insertedClass parentOfInsertedClass childOfInsertedClass |
	refactoring := (rbClass className: #InsertedClass)
		               superclass: #RBAbstractRefactoringTest;
		               subclasses: { self class };
		               packageName: #'Refactoring-Testing'.
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

{ #category : 'tests' }
RBInsertClassParametrizedTest >> testModelInsertClass [

	| refactoring insertedClass superClass subclass |
	superClass := model classNamed: #Foo.
	subclass := model classNamed: #Bar.

	refactoring := (rbClass model: model className: #InsertedClass)
		               superclass: superClass name;
		               subclasses: { subclass };
		               packageName: #'Refactoring-Testing'.

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
