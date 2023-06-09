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
	model := self abstractVariableTestData
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
RBAddClassVariableParametrizedTest >> testAlreadyExistingName [

	self shouldFail:
		(self createRefactoringWithArguments:
			{ #RecursiveSelfRule . #RBTransformationRuleTestData }).
	self shouldFail:
		(self createRefactoringWithArguments:
			{ self objectClassVariable . #RBTransformationRuleTestData })
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testMetaClassFailure [
	self flag: 'Broken ref2 test'
"	self shouldFail: (self createRefactoringWithArguments: { #VariableName . #'RBTransformationRuleTestData class'})"
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testMetaclass [
	| refactoring |
	self flag: 'Broken ref2 test'.
	refactoring := self createRefactoringWithModel: model andArguments: { #ClassVarName3 . #'Object class' }.
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

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testModelAlreadyExistingName [
	self shouldFail:
		(self createRefactoringWithModel: model
		andArguments: {#ClassVarName2 . #Bar })
]

{ #category : #'failure tests' }
RBAddClassVariableParametrizedTest >> testModelMetaclass [
	| refactoring |
	self flag: 'Broken ref2 test'.
	refactoring := self createRefactoringWithModel: model andArguments: { #ClassVarName3 . #'Bar class' }.
"	self shouldFail: refactoring"
]
