Class {
	#name : #RBAddClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBAddClassVariableTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddClassVariableTest >> testAddClassVariable [
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
		variable: 'Asdf'
		class: RBTransformationRuleTest.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBTransformationRuleTest) directlyDefinesClassVariable: #Asdf)
]

{ #category : #'failure tests' }
RBAddClassVariableTest >> testAlreadyExistingName [
	self
		shouldFail: (RBAddClassVariableRefactoring 
				variable: #RecursiveSelfRule
				class: RBTransformationRuleTest);
		shouldFail: (RBAddClassVariableRefactoring 
				variable: self objectClassVariable
				class: RBTransformationRuleTest)
]

{ #category : #'failure tests' }
RBAddClassVariableTest >> testMetaClassFailure [
	self shouldFail: (RBAddClassVariableRefactoring 
			variable: #VariableName
			class: RBTransformationRuleTest class)
]

{ #category : #'failure tests' }
RBAddClassVariableTest >> testMetaclass [
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
		variable: #ClassVarName3
		class: (model classNamed: #Object) classSide.
	refactoring model: model.
	self shouldFail: refactoring
]

{ #category : #tests }
RBAddClassVariableTest >> testModelAddClassVariable [
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
				model: model
				variable: #ClassVarName3
				class: (model classNamed: #Bar).
	self executeRefactoring: refactoring.
	self assert: ((model classNamed: #Bar) 
				directlyDefinesClassVariable: #ClassVarName3)
]

{ #category : #'failure tests' }
RBAddClassVariableTest >> testModelAlreadyExistingName [
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
				model: model
				variable: #ClassVarName2
				class: (model classNamed: #Bar).
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddClassVariableTest >> testModelMetaclass [
	| refactoring |
	refactoring := RBAddClassVariableRefactoring 
		model: model
		variable: #ClassVarName3
		class: (model classNamed: #Bar) classSide.
	self shouldFail: refactoring
]
