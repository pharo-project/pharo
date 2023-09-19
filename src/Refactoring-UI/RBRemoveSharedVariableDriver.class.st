"
I'm a model for user interaction for the remove shared variable refactoring.
"
Class {
	#name : 'RBRemoveSharedVariableDriver',
	#superclass : 'RBDriver',
	#instVars : [
		'class',
		'variables',
		'refactoring'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
RBRemoveSharedVariableDriver >> refactoring [

	refactoring := RBRemoveSharedVariableRefactoring
		               model: model
		               remove: variables first
		               from: class
]

{ #category : 'execution' }
RBRemoveSharedVariableDriver >> runRefactoring [

	self refactoring.
	[
	[
	| changes |
	changes := refactoring generateChanges.
	self openPreviewWithChanges: changes ]
		on: RBApplicabilityChecksFailedError
		do: [ :err | ^ RBRefactoringError signal: err messageText ] ]
		on: RBBreakingChangeChecksFailedWarning
		do: [ :err |
			| dialog |
			dialog := SpConfirmDialog new
				          title: 'Methods still accessing variables';
				          label: 'Do you want to browse them?';
				          acceptLabel: 'Sure!';
				          cancelLabel: 'No, forget it';
				          openModal.
			dialog ifTrue: [
				self furtherActionFor:
					(RBSharedVariableStillReferenced new refactoring: refactoring) ].
			err return ]
]

{ #category : 'initialization' }
RBRemoveSharedVariableDriver >> scopes: refactoringScopes variable: aVariable for: aClass [
	
	scopes := refactoringScopes.
	variables := {aVariable}.
	class := aClass
]
