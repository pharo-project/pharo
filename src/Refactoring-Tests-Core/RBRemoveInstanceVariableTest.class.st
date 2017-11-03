Class {
	#name : #RBRemoveInstanceVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #Refactoring-Tests-Core
}

{ #category : #tests }
RBRemoveInstanceVariableTest >> testModelRemoveInstanceVariable [
	| class |
	model 
		defineClass: 'nil subclass: #Object instanceVariableNames: ''foo1'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Object.
	self assert: (class definesInstanceVariable: 'foo1').
	self executeRefactoring: (RBRemoveInstanceVariableRefactoring 
				model: model
				variable: 'foo1'
				class: class).
	self deny: (class definesInstanceVariable: 'foo1')
]

{ #category : #'failure tests' }
RBRemoveInstanceVariableTest >> testNonExistantName [
	self shouldFail: (RBRemoveInstanceVariableRefactoring 
			variable: 'name1'
			class: RBLintRuleTest)
]

{ #category : #'failure tests' }
RBRemoveInstanceVariableTest >> testReferencedVariable [
	self shouldFail: (RBRemoveInstanceVariableRefactoring 
			variable: 'name'
			class: RBLintRuleTest)
]

{ #category : #tests }
RBRemoveInstanceVariableTest >> testRemoveInstVar [
	| refactoring class |
	refactoring := RBRemoveInstanceVariableRefactoring 
		variable: 'foo1'
		class: RBLintRuleTest.
	class := refactoring model classNamed: #RBLintRuleTest.
	self assert: (class definesInstanceVariable: 'foo1').
	self executeRefactoring: refactoring.
	self deny: (class definesInstanceVariable: 'foo1')
]
