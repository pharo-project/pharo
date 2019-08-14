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

{ #category : #tests }
RBReplaceCalledMethodByAnotherTest >> testReplaceMethod [
	| refactoring count |
	count := (model allReferencesTo: #newResultClass:) size.
	model allReferencesTo: #anInstVar: do: [:method | count := count + 1].
	refactoring := RBReplaceCalledMethodByAnotherRefactoring  
				model: model
				replaceMethod: #anInstVar:
				in: RBBasicLintRuleTestData
				to: #newResultClass:
				permutation: (1 to: 1).
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	model allReferencesTo: #newResultClass:
		do: 
			[:method | 
			count := count - 1].
	self assert: count equals: (model allReferencesTo: #anInstVar:) size
]

{ #category : #tests }
RBReplaceCalledMethodByAnotherTest >> testSelectorsNotHaveSameNumberOfArgs [
	self shouldFail: (RBReplaceCalledMethodByAnotherRefactoring 
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23
			permutation: (1 to: 1))
]
