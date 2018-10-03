Class {
	#name : #NECTypedModelTest,
	#superclass : #TestCase,
	#category : #'NECompletion-Tests'
}

{ #category : #tests }
NECTypedModelTest >> testMessage [
	| model |
	model := NECTypedModel class: NECTestClass.
	self shouldnt: model hasMessage.
	model narrowWith: 'hagadagadu'.
	self assert: model hasMessage.
	self assert: model message = 'no completions found'
]

{ #category : #tests }
NECTypedModelTest >> testTitle [
	| model |
	model := NECTypedModel class: NECTestClass.
	self assert: #NECTestClass = model title
]

{ #category : #tests }
NECTypedModelTest >> testTyped [
	| model selectors |
	model := NECTypedModel class: NECTestClass.
	self assert: (model entriesOfType: #locals) size == 0.
	self assert: (model entriesOfType: #instance) size == 0.
	selectors := model entriesOfType: #selector.
	self assert: selectors size > 0.
	self assert: (selectors includes: #initialize).
	self assert: (selectors includes: #lowPriorityOverrides:).
	self assert: (selectors includes: #testIt:).
	model narrowWith: 'low'.
	selectors := model entriesOfType: #selector.
	self deny: (selectors includes: #initialize).
	self assert: (selectors includes: #lowPriorityOverrides:).
	self deny: (selectors includes: #testIt:)
]
