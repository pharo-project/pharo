Class {
	#name : #RBMergeInstanceVariableIntoAnotherParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBMergeInstanceVariableIntoAnotherParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBMergeInstanceVariableIntoAnother };
		yourself
]

{ #category : #'failure tests' }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testEqualsVariables [
	self shouldFail: (rbClass 
			rename: 'classBlock'
			to: 'classBlock'
			in: RBBasicLintRuleTestData)
]

{ #category : #'failure tests' }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testNonExistingVariable [
	self shouldFail: (rbClass 
			rename: 'classBlock'
			to: 'anInstVar1'
			in: RBBasicLintRuleTestData)
]

{ #category : #tests }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testReplaceVariableIntoAnother [
	| refactoring class |
	refactoring := rbClass 
			rename: 'classBlock'
			to: 'anInstVar'
			in: RBBasicLintRuleTestData.
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class whichSelectorsReferToInstanceVariable: 'classBlock') isNotEmpty.
	self executeRefactoring: refactoring.
	
	self assert: (class whichSelectorsReferToInstanceVariable: 'classBlock') isEmpty.
]

{ #category : #tests }
RBMergeInstanceVariableIntoAnotherParametrizedTest >> testReplaceVariableIntoSupeclassVariable [
	| refactoring class |
	refactoring := rbClass 
			rename: 'name'
			to: 'classBlock'
			in: RBBasicLintRuleTestData.
	class := refactoring model classNamed: #RBBasicLintRuleTestData.
	self assert: (class whichSelectorsReferToInstanceVariable: 'name') isNotEmpty.
	self executeRefactoring: refactoring.
	self assert: (class definesInstanceVariable: 'name').
	self assert: (class directlyDefinesInstanceVariable: 'classBlock').
	
	self assert: (class whichSelectorsReferToInstanceVariable: 'name') isEmpty.
]
