Class {
	#name : #RBAddMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #running }
RBAddMethodTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddMethodTest >> testAddMethod [
	| refactoring |
	refactoring := RBAddMethodRefactoring 
		addMethod: 'printString1 ^super printString'
		toClass: RBBasicLintRuleTestData
		inProtocols: #(#accessing ).
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData) parseTreeFor: #printString1) 
		equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #'failure tests' }
RBAddMethodTest >> testBadMethod [
	self shouldFail: (RBAddMethodRefactoring 
			addMethod: 'asdf ^super ^printString'
			toClass: RBBasicLintRuleTestData
			inProtocols: #(#accessing ))
]

{ #category : #'failure tests' }
RBAddMethodTest >> testExistingSelector [
	self shouldFail: (RBAddMethodRefactoring 
			addMethod: 'printString ^super printString'
			toClass: RBBasicLintRuleTestData
			inProtocols: #(#accessing ))
]

{ #category : #tests }
RBAddMethodTest >> testModelAddMethod [
	| refactoring class |
	class := model metaclassNamed: #Bar.
	refactoring := RBAddMethodRefactoring 
				model: model
				addMethod: 'printString1 ^super printString'
				toClass: class
				inProtocols: #(#accessing).
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #printString1) 
		  equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #'failure tests' }
RBAddMethodTest >> testModelExistingSelector [
	| refactoring |
	refactoring := RBAddMethodRefactoring 
				model: model
				addMethod: 'classVarName1 ^super printString'
				toClass: (model classNamed: #Bar)
				inProtocols: #(#accessing).
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddMethodTest >> testModelInheritedSelector [
	| refactoring |
	refactoring := RBAddMethodRefactoring 
				model: model
				addMethod: 'printString ^super printString'
				toClass: (model classNamed: #Bar)
				inProtocols: #(#accessing).
	self shouldFail: refactoring
]
