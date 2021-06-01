Class {
	#name : #RBPullUpVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBPullUpVariableTransformationTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBPullUpVariableTransformationTest >> testClassVariable [

	| refactoring |
	refactoring := (RBPullUpVariableTransformation
						classVariable: #RecursiveSelfRule
						class: #RBDummyLintRuleTest)
						asRefactoring.	
	
	[ refactoring transform ] on: RBRefactoringError do: [ :e | e resume ].
	self assert: ((refactoring model classNamed: #RBDummyLintRuleTest)
			directlyDefinesClassVariable: #RecursiveSelfRule).
	self deny: ((refactoring model classNamed: #RBTransformationDummyRuleTest)
			directlyDefinesClassVariable: #RecursiveSelfRule)
]

{ #category : #tests }
RBPullUpVariableTransformationTest >> testMetaclassFailure [

	self shouldFail: (RBPullUpVariableTransformation
							classVariable: #RecursiveSelfRule
							class: RBDummyLintRuleTest class)
							asRefactoring
]

{ #category : #tests }
RBPullUpVariableTransformationTest >> testRefactoring [

	| refactoring |
	refactoring := (RBPullUpVariableTransformation
						instanceVariable: 'result'
						class: #RBDummyLintRuleTest)
						asRefactoring.
	
	[ refactoring transform ] on: RBRefactoringError do: [ :e | e resume ].
	self assert: ((refactoring model classNamed: #RBDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result').
	
	self deny: ((refactoring model classNamed: #RBBasicDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result').
	self deny: ((refactoring model classNamed: #RBFooDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result')
]

{ #category : #tests }
RBPullUpVariableTransformationTest >> testTransform [

	| transformation |
	transformation := (RBPullUpVariableTransformation
							instanceVariable: 'result'
							class: #RBDummyLintRuleTest)
							transform.
	
	self assert: ((transformation model classNamed: #RBDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result').
	
	self deny: ((transformation model classNamed: #RBBasicDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result').
	self deny: ((transformation model classNamed: #RBFooDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result')
]
