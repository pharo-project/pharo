Class {
	#name : #RBReplaceMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #'failure tests' }
RBReplaceMethodTest >> testNotUnderstandNewSelector [
	self shouldFail: (RBReplaceMethodRefactoring 
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23:
			permutation: (1 to: 1))
]

{ #category : #'failure tests' }
RBReplaceMethodTest >> testNotUnderstandSelector [
	self shouldFail: (RBReplaceMethodRefactoring 
			replaceCallMethod: #checkClass123:
			in: RBBasicLintRuleTestData
			to: #checkMethod:
			permutation: (1 to: 1))
]

{ #category : #tests }
RBReplaceMethodTest >> testReplaceMethod [
	| refactoring count |
	count := (model allReferencesTo: #newResultClass:) size.
	model allReferencesTo: #anInstVar: do: [:method | count := count + 1].
	refactoring := RBReplaceMethodRefactoring  
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

{ #category : #'failure tests' }
RBReplaceMethodTest >> testSelectorsNotHaveSameNumberOfArgs [
	self shouldFail: (RBReplaceMethodRefactoring 
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23
			permutation: (1 to: 1))
]
