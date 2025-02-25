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

{ #category : 'running' }
RBPullUpClassVariableParametrizedTest >> setUp [

	| classesInModel |
	super setUp.
	classesInModel := { RBLintRuleTestData } asOrderedCollection
		                  addAll: RBLintRuleTestData allSubclasses;
		                  yourself.
	model := self modelOnClasses: classesInModel
]

{ #category : 'failure tests' }
RBPullUpClassVariableParametrizedTest >> testFailureMetaClassFailure [

	self shouldFail: (self createRefactoringWithModel: model andArguments:
			 { #RecursiveSelfRule. #'RBLintRuleTestData class' })
]

{ #category : 'failure tests' }
RBPullUpClassVariableParametrizedTest >> testFailureNonExistantName [

	self shouldFail:
		(self createRefactoringWithModel: model andArguments: { #Foo. #RBLintRuleTestData })
]

{ #category : 'tests' }
RBPullUpClassVariableParametrizedTest >> testPullUpClassVariable [
	| refactoring |
	refactoring := self createRefactoringWithModel: model andArguments:
		{ #RecursiveSelfRule . #RBLintRuleTestData }.
	[ self executeRefactoring: refactoring ]
		on: RBRefactoringWarning 
		do: [ :e | e resume ].
	self assert: ((refactoring model classNamed: #RBLintRuleTestData)
		directlyDefinesClassVariable: #RecursiveSelfRule).
	self deny: ((refactoring model classNamed: #RBTransformationRuleTestData)
		directlyDefinesClassVariable: #RecursiveSelfRule)
]
