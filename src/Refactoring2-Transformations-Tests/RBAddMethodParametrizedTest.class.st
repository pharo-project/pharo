Class {
	#name : #RBAddMethodParametrizedTest,
	#superclass : #RBParametrizedTestWithDifferentConstructors,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBAddMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddMethodRefactoring .
					  #constructor -> #addMethod:toClass:inProtocols: };
		addCase: { #rbClass -> RBAddMethodTransformation .
					  #constructor -> #sourceCode:in:withProtocols: };
		yourself
]

{ #category : #running }
RBAddMethodParametrizedTest >> setUp [
	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddMethodParametrizedTest >> testAddMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{'printString1 ^super printString' . RBBasicLintRuleTestData . #(#accessing )}.
	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData) parseTreeFor: #printString1) 
		equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testBadMethod [
	self shouldFail: (self createRefactoringWithArguments: 
		{ 'asdf ^super ^printString' . RBBasicLintRuleTestData . #(#accessing )})
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testExistingSelector [
	self shouldFail: (self createRefactoringWithArguments: 
		{'printString ^super printString' . RBBasicLintRuleTestData . #(#accessing )})
]

{ #category : #tests }
RBAddMethodParametrizedTest >> testModelAddMethod [
	| refactoring class |
	class := model metaclassNamed: #Bar.
	refactoring := self createRefactoringWithModel: model andArguments: 
				{'printString1 ^super printString' . class . #(#accessing)}.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeFor: #printString1) 
		  equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testModelExistingSelector [
	| refactoring |
	refactoring := self createRefactoringWithModel: model andArguments: 
				{'classVarName1 ^super printString' . (model classNamed: #Bar) . #(#accessing) }.
	self shouldFail: refactoring
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testModelInheritedSelector [
	| refactoring |
	refactoring := self createRefactoringWithModel: model andArguments: 
				{ 'printString ^super printString' . (model classNamed: #Bar) . #(#accessing) }.
	self shouldFail: refactoring
]
