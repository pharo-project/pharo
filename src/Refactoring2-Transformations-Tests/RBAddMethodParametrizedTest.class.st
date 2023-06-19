Class {
	#name : #RBAddMethodParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
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
	model := self rbModelForVariableTest
]

{ #category : #tests }
RBAddMethodParametrizedTest >> testAddMethod [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{'printString1 ^ super printString' .
		RBBasicLintRuleTestData .
		#(#accessing )}.

	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData)
			parseTreeForSelector: #printString1)
		equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testFailureBadMethod [

	self shouldFail: (self createRefactoringWithArguments: {
				 'asdf ^super ^printString'.
				 RBBasicLintRuleTestData.
				 #( #accessing ) })
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testFailureExistingSelector [
	self shouldFail: (self createRefactoringWithArguments:
		{'printString ^super printString' .
		RBBasicLintRuleTestData .
		#(#accessing )})
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testFailureModelExistingSelector [

	self
		shouldFail: (self createRefactoringWithModel: model andArguments: {
						 'classVarName1 ^super printString'.
						 (model classNamed: #Bar).
						 #( #accessing ) });
		shouldFail: (self createRefactoringWithModel: model andArguments: {
						 'printString ^super printString'.
						 (model classNamed: #Bar).
						 { #accessing } })
]

{ #category : #'failure tests' }
RBAddMethodParametrizedTest >> testFailureModelInheritedSelector [

	| refactoring |
	refactoring := self
		               createRefactoringWithModel: model
		               andArguments: {
				               'printString ^super printString'.
				               (model classNamed: #Bar).
				               #( #accessing ) }.
	self shouldFail: refactoring
]

{ #category : #tests }
RBAddMethodParametrizedTest >> testModelAddMethod [
	| refactoring class |
	class := model metaclassNamed: #Bar.
	refactoring := self createRefactoringWithModel: model andArguments:
				{'printString1 ^super printString' .
				class .
				#(#accessing)}.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeForSelector: #printString1)
		  equals: (self parseMethod: 'printString1 ^super printString')
]
