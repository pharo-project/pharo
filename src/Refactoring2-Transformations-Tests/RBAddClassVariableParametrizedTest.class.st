Class {
	#name : #RBAddClassVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddClassVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBAddVariableTransformation .
					  #constructor -> #classVariable:class: };
		yourself
]

{ #category : #running }
RBAddClassVariableParametrizedTest >> setUp [
	super setUp.
	model := self rbModelForVariableTest
]

{ #category : #tests }
RBAddClassVariableParametrizedTest >> testAddClassVariable [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{'Asdf' . #RBTransformationRuleTestData }.

	self executeRefactoring: refactoring.

	self assert: ((refactoring model
		classNamed: #RBTransformationRuleTestData)
		directlyDefinesClassVariable: #Asdf)
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testFailureAlreadyExistingName [

	self shouldFail: (self createRefactoringWithArguments:
			 { #RecursiveSelfRule. #RBTransformationRuleTestData }).
	self shouldFail: (self createRefactoringWithArguments: {
				 self objectClassVariable.
				 #RBTransformationRuleTestData })
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testFailureMetaClassFailure [

	self skip.
	"	self shouldFail: (self createRefactoringWithArguments: { #VariableName . #'RBTransformationRuleTestData class'})"
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testFailureMetaclass [

	| refactoring |
	self skip.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: { #ClassVarName3. #'Object class' }
	"	self shouldFail: refactoring"
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testFailureModelAlreadyExistingName [

	self shouldFail: (self
			 createRefactoringWithModel: model
			 andArguments: { #ClassVarName2. #Bar })
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testFailureModelMetaclass [

	| refactoring |
	self skip.
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: { #ClassVarName3. #'Bar class' }
	"	self shouldFail: refactoring"
]

{ #category : #tests }
RBAddClassVariableParametrizedTest >> testModelAddClassVariable [
	| refactoring |
	refactoring := self createRefactoringWithModel: model
		andArguments: { #ClassVarName3 . #Bar }.

	self executeRefactoring: refactoring.

	self assert: ((model classNamed: #Bar)
				directlyDefinesClassVariable: #ClassVarName3)
]
