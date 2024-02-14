Class {
	#name : 'RBPullUpClassVariableParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#category : 'Refactoring-Transformations-Tests-Parametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'tests' }
RBPullUpClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPullUpClassVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPullUpVariableTransformation .
					  #constructor -> #classVariable:class: };
		yourself
]

{ #category : 'failure tests' }
RBPullUpClassVariableParametrizedTest >> testFailureMetaClassFailure [

	self shouldFail: (self createRefactoringWithArguments:
			 { #RecursiveSelfRule. #'RBLintRuleTestData class' })
]

{ #category : 'failure tests' }
RBPullUpClassVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail:
		(self createRefactoringWithArguments: { #Foo. #RBLintRuleTestData })
]

{ #category : 'tests' }
RBPullUpClassVariableParametrizedTest >> testPullUpClassVariable [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ #RecursiveSelfRule . #RBLintRuleTestData }.
	[ self executeRefactoring: refactoring ]
		on: RBRefactoringWarning 
		do: [ :e | e resume ].
	self assert: ((refactoring model classNamed: #RBLintRuleTestData)
		directlyDefinesClassVariable: #RecursiveSelfRule).
	self deny: ((refactoring model classNamed: #RBTransformationRuleTestData)
		directlyDefinesClassVariable: #RecursiveSelfRule)
]
