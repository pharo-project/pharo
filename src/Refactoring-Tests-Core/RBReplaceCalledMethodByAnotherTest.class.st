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
RBReplaceCalledMethodByAnotherTest >> testReplacePrimitive [
	"| refactoring count |
	count := 0.
	model allReferencesTo: #anInstVar: do: [:method | count := count + 1].
	refactoring := RBReplaceCalledMethodByAnotherRefactoring  
				model: model
				replaceMethod: #anInstVar:
				in: RBBasicLintRuleTestData
				to: #newResultClass:
				permutation: (1 to: 2).
	self proceedThroughWarning: [self executeRefactoring: refactoring]."
]

{ #category : #tests }
RBReplaceCalledMethodByAnotherTest >> testSelectorsNotHaveSameNumberOfArgs [
	self shouldFail: (RBReplaceCalledMethodByAnotherRefactoring 
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23
			permutation: (1 to: 1))
]
