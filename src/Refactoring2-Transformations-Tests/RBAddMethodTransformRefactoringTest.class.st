Class {
	#name : #RBAddMethodTransformRefactoringTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #'building suites' }
RBAddMethodTransformRefactoringTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddMethodTransformRefactoring .
					  #constructor -> #addMethod:toClass:inProtocols: };
		yourself
]

{ #category : #running }
RBAddMethodTransformRefactoringTest >> setUp [
	super setUp.
	model := self rbModelForVariableTest
]

{ #category : #tests }
RBAddMethodTransformRefactoringTest >> testAddMethod [
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
RBAddMethodTransformRefactoringTest >> testFailureBadMethod [

	self shouldFail: (self createRefactoringWithArguments: {
				 'asdf ^super ^printString'.
				 RBBasicLintRuleTestData.
				 #( #accessing ) })
]

{ #category : #tests }
RBAddMethodTransformRefactoringTest >> testModelAddMethod [
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
