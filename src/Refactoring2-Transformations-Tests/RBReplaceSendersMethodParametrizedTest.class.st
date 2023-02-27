Class {
	#name : #RBReplaceSendersMethodParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #tests }
RBReplaceSendersMethodParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBReplaceMessageSendTransformation };
		yourself
]

{ #category : #'failure tests' }
RBReplaceSendersMethodParametrizedTest >> testIncompleteInitializers [
	self shouldFail: (rbClass
				model: model
				replaceMethod: ('called:', 'on:') asSymbol
				in: RBRefactoryTestDataApp
				to: ('textInput:', 'name:', 'symbol:') asSymbol
				permutation: #(-1 2 1)
				inAllClasses: true)
]

{ #category : #tests }
RBReplaceSendersMethodParametrizedTest >> testModelReplaceMethodOnlyInClass [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('result', 'Class:') asSymbol.
	newSelector := ('method', 'Block:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := ((model allReferencesTo: selector) select: [ :e | e modelClass name = 'RBBasicLintRuleTestData' ] ) size.
	refactoring := rbClass
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

{ #category : #tests }
RBReplaceSendersMethodParametrizedTest >> testModelReplaceMethodWithLessArgs [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('called:', 'on:') asSymbol.
	newSelector := ('foo:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := (model allReferencesTo: selector) size.
	refactoring := rbClass
				model: model
				replaceMethod: selector
				in: RBRefactoryTestDataApp
				to: newSelector
				permutation: #(2)
				inAllClasses: true.
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (model allReferencesTo: newSelector) size
]

{ #category : #tests }
RBReplaceSendersMethodParametrizedTest >> testModelReplaceMethodWithMoreArgs [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('called:', 'on:') asSymbol.
	newSelector := ('textInput:', 'name:', 'symbol:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := (model allReferencesTo: selector) size.
	refactoring := rbClass
				model: model
				replaceMethod: selector
				in: RBRefactoryTestDataApp
				to: newSelector
				permutation: #(-1 2 1)
				inAllClasses: true.
	refactoring initializers: #('OrderedCollection new: 5').
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (model allReferencesTo: newSelector) size
]

{ #category : #tests }
RBReplaceSendersMethodParametrizedTest >> testModelReplaceMethodWithSameArgs [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('called:', 'on:') asSymbol.
	newSelector := ('called:', 'on1:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := (model allReferencesTo: selector) size.
	refactoring := rbClass
				model: model
				replaceMethod: selector
				in: RBRefactoryTestDataApp
				to: newSelector
				permutation: #(2 1)
				inAllClasses: true.
	self proceedThroughWarning: [ refactoring primitiveExecute ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (model allReferencesTo: newSelector) size
]

{ #category : #'failure tests' }
RBReplaceSendersMethodParametrizedTest >> testNotUnderstandNewSelector [
	self shouldFail: (rbClass
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23:
			permutation: (1 to: 1))
]

{ #category : #'failure tests' }
RBReplaceSendersMethodParametrizedTest >> testNotUnderstandSelector [
	self shouldFail: (rbClass
			replaceCallMethod: #checkClass123:
			in: RBBasicLintRuleTestData
			to: #checkMethod:
			permutation: (1 to: 1))
]

{ #category : #tests }
RBReplaceSendersMethodParametrizedTest >> testReplaceMethodInAllClasses [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('an', 'InstVar:') asSymbol.
	newSelector := ('new', 'ResultClass:') asSymbol.
	refactoring := rbClass
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
RBReplaceSendersMethodParametrizedTest >> testReplaceMethodOnlyInClass [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('result', 'Class:') asSymbol.
	newSelector := ('method', 'Block:') asSymbol.
	refactoring := rbClass
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
