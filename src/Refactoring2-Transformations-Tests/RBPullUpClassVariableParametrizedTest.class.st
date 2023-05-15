Class {
	#name : #RBPullUpClassVariableParametrizedTest,
	#superclass : #RBWithDifferentConstructorsParametrizedTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBPullUpClassVariableParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBPullUpClassVariableRefactoring .
					  #constructor -> #variable:class: };
		addCase: { #rbClass -> RBPullUpVariableTransformation .
					  #constructor -> #classVariable:class: };
		yourself
]

{ #category : #'failure tests' }
RBPullUpClassVariableParametrizedTest >> testMetaClassFailure [
	self shouldFail:
		(self createRefactoringWithArguments:
			{ #RecursiveSelfRule . #'RBLintRuleTestData class'})
]

{ #category : #'failure tests' }
RBPullUpClassVariableParametrizedTest >> testNonExistantName [
self flag: 'TODO: Some conditions are in transformation part'.

	"self shouldFail: (self createRefactoringWithArguments:
		{ #Foo . #RBLintRuleTestData })"
]

{ #category : #tests }
RBPullUpClassVariableParametrizedTest >> testPullUpClassVariable [
	| refactoring |
	refactoring := self createRefactoringWithArguments:
		{ #RecursiveSelfRule . #RBLintRuleTestData }.
	[ self executeRefactoring: refactoring ]
		on: RBRefactoringError
		do: [ :e | e resume ].
	self assert: ((refactoring model classNamed: #RBLintRuleTestData)
		directlyDefinesClassVariable: #RecursiveSelfRule).
	self deny: ((refactoring model classNamed: #RBTransformationRuleTestData)
		directlyDefinesClassVariable: #RecursiveSelfRule)
]
