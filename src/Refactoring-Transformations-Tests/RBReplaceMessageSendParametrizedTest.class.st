Class {
	#name : 'RBReplaceMessageSendParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBReplaceMessageSendParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBReplaceMessageSendTransformation };
		yourself
]

{ #category : 'running' }
RBReplaceMessageSendParametrizedTest >> setUp [
	| classEnv |
	super setUp.
	classEnv := RBClassEnvironment classes: { RBReplaceMessageSendTransformation . RBClassDataForRefactoringTest . RBBasicLintRuleTestData }.
	model := self defaultNamespaceClass onEnvironment: classEnv
]

{ #category : 'failure tests' }
RBReplaceMessageSendParametrizedTest >> testFailureIncompleteInitializers [
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	
	self shouldFail: (rbClass
				model: model
				replaceMethod: ('called:', 'on:') asSymbol
				
				in: RBClassDataForRefactoringTest
				to: ('textInput:', 'name:', 'symbol:') asSymbol
				permutation: #(-1 2 1)
				inAllClasses: true)
]

{ #category : 'failure tests' }
RBReplaceMessageSendParametrizedTest >> testFailureNotUnderstandNewSelector [
	self shouldFail: (rbClass
			replaceCallMethod: #checkClass:
			in: RBBasicLintRuleTestData
			to: #checkMethod23:
			permutation: (1 to: 1))
]

{ #category : 'failure tests' }
RBReplaceMessageSendParametrizedTest >> testFailureNotUnderstandSelector [
	self shouldFail: (rbClass
			replaceCallMethod: #checkClass123:
			in: RBBasicLintRuleTestData
			to: #checkMethod:
			permutation: (1 to: 1))
]

{ #category : 'tests' }
RBReplaceMessageSendParametrizedTest >> testModelReplaceMethodOnlyInClass [
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
	self proceedThroughWarning: [ refactoring generateChanges ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (model allReferencesTo: newSelector) size
]

{ #category : 'tests' }
RBReplaceMessageSendParametrizedTest >> testModelReplaceMethodWithLessArgs [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('called:', 'on:') asSymbol.
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	newSelector := ('foo:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := (model allReferencesTo: selector) size.
	refactoring := rbClass
				model: model
				replaceMethod: selector
				in: RBClassDataForRefactoringTest
				to: newSelector
				permutation: #(2)
				inAllClasses: true.
	self proceedThroughWarning: [ refactoring generateChanges ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (model allReferencesTo: newSelector) size
]

{ #category : 'tests' }
RBReplaceMessageSendParametrizedTest >> testModelReplaceMethodWithMoreArgs [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('called:', 'on:') asSymbol.
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	newSelector := ('textInput:', 'name:', 'symbol:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := (model allReferencesTo: selector) size.
	refactoring := rbClass
				model: model
				replaceMethod: selector
				in: RBClassDataForRefactoringTest
				to: newSelector
				permutation: #(-1 2 1)
				inAllClasses: true.
	refactoring newArgs: { RBArgumentName name: 'anObject' value: 'OrderedCollection new: 5'}.
	self proceedThroughWarning: [ refactoring generateChanges ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (model allReferencesTo: newSelector) size
]

{ #category : 'tests' }
RBReplaceMessageSendParametrizedTest >> testModelReplaceMethodWithSameArgs [
	| refactoring sendersNewSelector sendersLastSelector selector newSelector |
	selector := ('called:', 'on:') asSymbol.
	"please note that this strange way to create symbol is because some tests are counting symbol
	and rewriting the code in a bad way so for now do not change them."
	newSelector := ('called:', 'on1:') asSymbol.
	sendersNewSelector := (model allReferencesTo: newSelector) size.
	sendersLastSelector := (model allReferencesTo: selector) size.
	refactoring := rbClass
				model: model
				replaceMethod: selector
				in: RBClassDataForRefactoringTest
				to: newSelector
				permutation: #(2 1)
				inAllClasses: true.
	self proceedThroughWarning: [ refactoring generateChanges ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (model allReferencesTo: newSelector) size
]

{ #category : 'tests' }
RBReplaceMessageSendParametrizedTest >> testReplaceMethodInAllClasses [
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
	self proceedThroughWarning: [ refactoring generateChanges ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (refactoring model allReferencesTo: newSelector) size
]

{ #category : 'tests' }
RBReplaceMessageSendParametrizedTest >> testReplaceMethodOnlyInClass [
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
	self proceedThroughWarning: [ refactoring generateChanges ].
	self assert: sendersNewSelector + sendersLastSelector
		equals: (refactoring model allReferencesTo: newSelector) size
]
