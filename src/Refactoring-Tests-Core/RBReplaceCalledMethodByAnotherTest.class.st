Class {
	#name : #RBReplaceCalledMethodByAnotherTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBReplaceCalledMethodByAnotherTest >> testNotUnderstandNewSelector [
	self shouldFail: (RBReplaceCalledMethodByAnotherRefactoring 
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23:
			permutation: (1 to: 1))
]

{ #category : #tests }
RBReplaceCalledMethodByAnotherTest >> testNotUnderstandSelector [
	self shouldFail: (RBReplaceCalledMethodByAnotherRefactoring 
			replaceCallMethod: #checkClass123:
			in: RBBasicLintRuleTestData
			to: #checkMethod:
			permutation: (1 to: 1))
]
