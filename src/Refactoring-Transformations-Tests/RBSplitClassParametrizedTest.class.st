Class {
	#name : 'RBSplitClassParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBSplitClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBSplitClassRefactoring };
	"	addCase: { #rbClass -> RBSplitClassTransformation };
		"yourself
]

{ #category : 'accessing' }
RBSplitClassParametrizedTest >> constructor [
	^ #class:instanceVariables:newClassName:referenceVariableName:
]

{ #category : 'failure tests' }
RBSplitClassParametrizedTest >> testFailureInvalidInstanceVariableName [

	self shouldFail: (self createRefactoringWithArguments: {
				 #RBBasicLintRuleTestData.
				 #( #inexistentVar #classBlock ).
				 #Object.
				 #blocks })
]

{ #category : 'failure tests' }
RBSplitClassParametrizedTest >> testFailureInvalidNewClassName [

	self shouldFail: (self createRefactoringWithArguments: {
				 #RBBasicLintRuleTestData.
				 #( #methodBlock #classBlock ).
				 #Object.
				 #blocks })
]

{ #category : 'failure tests' }
RBSplitClassParametrizedTest >> testFailureInvalidReferenceVariableName [

	self shouldFail: (self createRefactoringWithArguments: {
				 #RBLintRuleTestData.
				 #( #name #foo1 ).
				 #ClassA.
				 #result }).
	self shouldFail: (self createRefactoringWithArguments: {
				 #RBLintRuleTestData.
				 #( #name #foo1 ).
				 #ClassA.
				 #temp })
]

{ #category : 'tests' }
RBSplitClassParametrizedTest >> testSplitClass [
	|ref aModel|
	ref := self createRefactoringWithArguments:
		{ #RBBasicLintRuleTestData . #(#methodBlock #classBlock) . #BlockClass . #blocks }.
	aModel := ref model.
	self executeRefactoring: ref.

	self assertCollection: (aModel classNamed: #RBBasicLintRuleTestData) instanceVariableNames 			hasSameElements: #(#result #blocks #anInstVar).
	self assertCollection: (aModel classNamed: #BlockClass) instanceVariableNames
			hasSameElements: #(#methodBlock #classBlock).

	self assert: ((aModel classNamed: #RBBasicLintRuleTestData) parseTreeForSelector: #checkClass:)
				equals: (self parseMethod: 'checkClass: aSmalllintContext
	^blocks classBlock value: aSmalllintContext value: result').
	self assert: ((aModel classNamed: #RBBasicLintRuleTestData) parseTreeForSelector: #checkMethod:)
				equals: (self parseMethod: 'checkMethod: aSmalllintContext
	^blocks methodBlock value: aSmalllintContext value: result')
]
