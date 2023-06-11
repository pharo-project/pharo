Class {
	#name : #RBAddInstanceVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBAddVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : #running }
RBAddInstanceVariableParametrizedTest >> setUp [
	super setUp.
	model := self abstractVariableTestData
]

{ #category : #tests }
RBAddInstanceVariableParametrizedTest >> testAddInstanceVariable [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{'asdf' . #RBTransformationRuleTestData }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model
		classNamed: #RBTransformationRuleTestData)
		directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #tests }
RBAddInstanceVariableParametrizedTest >> testAddToModel [
	| refactoring |
	model := RBNamespace new.
	model name: 'Add instance variable'.
	model defineClass: 'Object << #FOOBAR
		slots: { #fdsa };
		package: #''Refactory-Test data'''.
	refactoring := self createRefactoringWithModel: model
		andArguments: {'asdf' .  #FOOBAR }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model
		classNamed: #FOOBAR)
		directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testFailureAlreadyExistingName [

	self shouldFail: (self createRefactoringWithArguments:
			 { 'class'. #RBTransformationRuleTestData }).
	self shouldFail: (self createRefactoringWithArguments:
			 { 'name'. #RBTransformationRuleTestData })
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testFailureModelAlreadyExistingName [

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { 'instVarName1'. #Bar })
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testFailureModelMetaclassAlreadyExistingName [

	(model metaclassNamed: #Foo) addInstanceVariable: 'instVarName1'.

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { 'instVarName1'. #Bar })
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testFailureNewExistingName [

	| refactoring |
	model := RBNamespace new.
	model name: 'Add instance variable'.
	model defineClass: 'Object << #FOOBAR
		slots: { #asdf };
		package: #''Refactory-Test data'''.

	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: { 'asdf'. #FOOBAR }.

	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddInstanceVariableParametrizedTest >> testFailureNewHierarchyExistingName [

	| refactoring |
	model := RBNamespace new.
	model name: 'Add instance variable'.


	model defineClass: 'Object << #FOOBAR
		slots: { #asdf };
		package: #''Refactory-Test data'''.

	model defineClass:
		'FOOBAR << #BARFOO package: ''Refactory-Test data'''.

	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: { 'asdf'. #BARFOO }.

	self shouldFail: refactoring
]
