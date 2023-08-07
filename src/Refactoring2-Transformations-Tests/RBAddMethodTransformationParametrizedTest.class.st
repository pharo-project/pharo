Class {
	#name : #RBAddMethodTransformationParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #'building suites' }
RBAddMethodTransformationParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddMethodTransformation .
					  #constructor -> #sourceCode:in:withProtocols: };
		addCase: { #rbClass -> RBAddMethodRefactoring .
					  #constructor -> #sourceCode:in:withProtocols: };
		yourself
]

{ #category : #running }
RBAddMethodTransformationParametrizedTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : #tests }
RBAddMethodTransformationParametrizedTest >> testAddMethod [
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
RBAddMethodTransformationParametrizedTest >> testFailureBadMethod [

	self shouldFail: (self createRefactoringWithArguments: {
				 'asdf ^super ^printString'.
				 RBBasicLintRuleTestData.
				 #( #accessing ) })
]

{ #category : #tests }
RBAddMethodTransformationParametrizedTest >> testModelAddMethod [
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
