Class {
	#name : 'RBAddMethodTransformationParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'building suites' }
RBAddMethodTransformationParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBAddMethodTransformation .
					  #constructor -> #sourceCode:in:withProtocol: };
		addCase: { #rbClass -> RBAddMethodRefactoring .
					  #constructor -> #sourceCode:in:withProtocol: };
		yourself
]

{ #category : 'running' }
RBAddMethodTransformationParametrizedTest >> setUp [

	super setUp.
	model := self modelOnClasses: { RBBasicLintRuleTestData }.
	self defineFooBarIn: model
]

{ #category : 'tests' }
RBAddMethodTransformationParametrizedTest >> testAddMethod [
	| refactoring |
	refactoring := self createRefactoringWithModel: model andArguments:
		{'printString1 ^ super printString' .
		RBBasicLintRuleTestData .
		#(#accessing )}.

	self executeRefactoring: refactoring.
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData)
			parseTreeForSelector: #printString1)
		equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : 'failure tests' }
RBAddMethodTransformationParametrizedTest >> testFailureBadMethod [
	self shouldFail: (self createRefactoringWithModel: model andArguments: {
				 'asdf ^super ^printString'.
				 RBBasicLintRuleTestData.
				 #( #accessing ) })
]

{ #category : 'tests' }
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
