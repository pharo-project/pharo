Class {
	#name : #StDebuggerContextInteractionModelTest,
	#superclass : #TestCase,
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #tests }
StDebuggerContextInteractionModelTest >> testHasUnsavedCodeChanges [

	| model code |
	model := StDebuggerContextInteractionModel new.
	model context: [  ] asContext.
	code := SpCodePresenter new text: [  ] asContext sourceCode.
	model owner: code.
	
	self deny: model hasUnsavedCodeChanges.
	
	code text: 'changed source'.
	self assert: model hasUnsavedCodeChanges
]

{ #category : #tests }
StDebuggerContextInteractionModelTest >> testNilContextModel [

	self deny:
		StDebuggerContextInteractionModel new hasUnsavedCodeChanges
]
