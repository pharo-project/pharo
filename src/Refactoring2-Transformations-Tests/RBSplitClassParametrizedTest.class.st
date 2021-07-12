Class {
	#name : #RBSplitClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBSplitClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBSplitClassRefactoring };
	"	addCase: { #rbClass -> RBSplitClassTransformation };
		"yourself
]

{ #category : #'failure tests' }
RBSplitClassParametrizedTest >> testInvalidInstanceVariableName [
	self shouldFail: 
		(rbClass 
			class: #RBBasicLintRuleTestData 
			instanceVariables: #(#inexistentVar #classBlock) 
			newClassName: #Object 
			referenceVariableName: #blocks)
]

{ #category : #'failure tests' }
RBSplitClassParametrizedTest >> testInvalidNewClassName [
	self shouldFail: 
		(rbClass 
			class: #RBBasicLintRuleTestData 
			instanceVariables: #(#methodBlock #classBlock) 
			newClassName: #Object 
			referenceVariableName: #blocks)
]

{ #category : #'failure tests' }
RBSplitClassParametrizedTest >> testInvalidReferenceVariableName [
	self shouldFail: 
		(rbClass 
			class: #RBLintRuleTestData 
			instanceVariables: #(#name #foo1) 
			newClassName: #ClassA 
			referenceVariableName: #result).
	self shouldFail: 
		(rbClass 
			class: #RBLintRuleTestData 
			instanceVariables: #(#name #foo1) 
			newClassName: #ClassA 
			referenceVariableName: #temp).
]

{ #category : #tests }
RBSplitClassParametrizedTest >> testSplitClass [
	|ref aModel|
	ref := rbClass 
		class: #RBBasicLintRuleTestData 
		instanceVariables: #(#methodBlock #classBlock) 
		newClassName: #BlockClass 
		referenceVariableName: #blocks.
	aModel := ref model.
	self executeRefactoring: ref.
	
	self assertCollection: (aModel classNamed: #RBBasicLintRuleTestData) instanceVariableNames 			hasSameElements: #(#result #blocks #anInstVar).
	self assertCollection: (aModel classNamed: #BlockClass) instanceVariableNames 
			hasSameElements: #(#methodBlock #classBlock).
			
	self assert: ((aModel classNamed: #RBBasicLintRuleTestData) parseTreeFor: #checkClass:) 
				equals: (self parseMethod: 'checkClass: aSmalllintContext 
	^blocks classBlock value: aSmalllintContext value: result').
	self assert: ((aModel classNamed: #RBBasicLintRuleTestData) parseTreeFor: #checkMethod:) 
				equals: (self parseMethod: 'checkMethod: aSmalllintContext 
	^blocks methodBlock value: aSmalllintContext value: result')
]
