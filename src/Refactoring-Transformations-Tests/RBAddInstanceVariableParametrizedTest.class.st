Class {
	#name : 'RBAddInstanceVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBAddInstanceVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddInstanceVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBAddVariableTransformation .
					  #constructor -> #instanceVariable:class: };
		yourself
]

{ #category : 'running' }
RBAddInstanceVariableParametrizedTest >> setUp [
	super setUp.
	model := self modelOnClasses: { RBTransformationRuleTestData }
]

{ #category : 'tests' }
RBAddInstanceVariableParametrizedTest >> testAddInstanceVariable [

	| refactoring |
	
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments:
		               { 'asdf'. #RBTransformationRuleTestData }.

	self executeRefactoring: refactoring.

	self assert:
		((refactoring model classNamed: #RBTransformationRuleTestData)
			 directlyDefinesInstanceVariable: 'asdf')
]

{ #category : 'tests' }
RBAddInstanceVariableParametrizedTest >> testAddToModel [

	| refactoring |
	
	model name: 'Add instance variable'.
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #fdsa };
			package: #'Refactory-Test data' ].
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: { 'asdf'. #FOOBAR }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model classNamed: #FOOBAR)
			 directlyDefinesInstanceVariable: 'asdf')
]

{ #category : 'tests' }
RBAddInstanceVariableParametrizedTest >> testInvalidVariableName [

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { ''. #RBTransformationRuleTestData }).

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { 'true'. #RBTransformationRuleTestData }).

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { 'Asdf'. #RBTransformationRuleTestData })
]
