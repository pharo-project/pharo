Class {
	#name : #RBReplaceMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core-Refactorings'
}

{ #category : #tests }
RBReplaceMethodTest >> testModelReplaceMethodInAllClasses [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('an', 'InstVar:') asSymbol.
	newSelector := ('new', 'ResultClass:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := (model allReferencesTo: selector) size.
	refactoring := RBReplaceMethodRefactoring  
				model: model
				replaceMethod: selector
				in: RBBasicLintRuleTestData
				to: newSelector 
				permutation: (1 to: 1)
				inAllClasses: true.
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	self assert: sendersNewSelector + sendersLastSelector 
		equals: (model allReferencesTo: newSelector) size
]

{ #category : #tests }
RBReplaceMethodTest >> testModelReplaceMethodOnlyInClass [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('result', 'Class:') asSymbol.
	newSelector := ('method', 'Block:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := ((model allReferencesTo: selector) select: [ :e | e modelClass name = 'RBBasicLintRuleTestData' ] ) size.
	refactoring := RBReplaceMethodRefactoring  
				model: model
				replaceMethod: selector
				in: RBBasicLintRuleTestData
				to: newSelector 
				permutation: (1 to: 1)
				inAllClasses: false.
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	self assert: sendersNewSelector + sendersLastSelector 
		equals: (model allReferencesTo: newSelector) size
]

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
RBReplaceMethodTest >> testReplaceMethodInAllClasses [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('an', 'InstVar:') asSymbol.
	newSelector := ('new', 'ResultClass:') asSymbol.
	refactoring := RBReplaceMethodRefactoring
				replaceCallMethod: selector
				in: RBBasicLintRuleTestData
				to: newSelector 
				permutation: (1 to: 1)
				inAllClasses: true.
	sendersNewSelector := (refactoring model allReferencesTo: newSelector) size.
	sendersLastSelector := (refactoring model allReferencesTo: selector) size.
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	self assert: sendersNewSelector + sendersLastSelector 
		equals: (refactoring model allReferencesTo: newSelector) size
]

{ #category : #tests }
RBReplaceMethodTest >> testReplaceMethodOnlyInClass [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('result', 'Class:') asSymbol.
	newSelector := ('method', 'Block:') asSymbol.
	refactoring := RBReplaceMethodRefactoring 
				replaceCallMethod: selector
				in: RBBasicLintRuleTestData
				to: newSelector 
				permutation: (1 to: 1)
				inAllClasses: false.
	sendersNewSelector := (refactoring model allReferencesTo: newSelector) size.
	sendersLastSelector := ((refactoring model allReferencesTo: selector) select: [ :e | e modelClass name = 'RBBasicLintRuleTestData' ]) size.
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	self assert: sendersNewSelector + sendersLastSelector 
		equals: (refactoring model allReferencesTo: newSelector) size
]

{ #category : #'failure tests' }
RBReplaceMethodTest >> testSelectorsNotHaveSameNumberOfArgs [
	self shouldFail: (RBReplaceMethodRefactoring 
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23
			permutation: (1 to: 1))
]
