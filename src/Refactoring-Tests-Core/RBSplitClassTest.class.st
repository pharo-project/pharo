Class {
	#name : #RBSplitClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #'failure tests' }
RBSplitClassTest >> testInvalidInstanceVariableName [
	self shouldFail: 
		(RBSplitClassRefactoring 
			class: RBBasicLintRuleTestData 
			instanceVariables: #(#inexistentVar #classBlock) 
			newClassName: #Object 
			referenceVariableName: #blocks)
]

{ #category : #'failure tests' }
RBSplitClassTest >> testInvalidNewClassName [
	self shouldFail: 
		(RBSplitClassRefactoring 
			class: RBBasicLintRuleTestData 
			instanceVariables: #(#methodBlock #classBlock) 
			newClassName: #Object 
			referenceVariableName: #blocks)
]

{ #category : #'failure tests' }
RBSplitClassTest >> testInvalidReferenceVariableName [
	self shouldFail: 
		(RBSplitClassRefactoring 
			class: RBLintRuleTestData 
			instanceVariables: #(#name #foo1) 
			newClassName: #ClassA 
			referenceVariableName: #result).
	self shouldFail: 
		(RBSplitClassRefactoring 
			class: RBLintRuleTestData 
			instanceVariables: #(#name #foo1) 
			newClassName: #ClassA 
			referenceVariableName: #temp).
]

{ #category : #tests }
RBSplitClassTest >> testSplitClass [
	|ref aModel|
	ref := RBSplitClassRefactoring class: RBBasicLintRuleTestData instanceVariables: #(#methodBlock #classBlock) newClassName: #BlockClass referenceVariableName: #blocks.
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
