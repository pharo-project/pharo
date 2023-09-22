"
I'm a model for user interaction for the remove instance variable refactoring.
"
Class {
	#name : 'RBRemoveInstanceVariablesDriver',
	#superclass : 'RBDriver',
	#instVars : [
		'class',
		'variables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
RBRemoveInstanceVariablesDriver >> configureRefactoring [

	refactoring := RBCompositeRefactoring new
							model: model; 
							refactorings: (variables collect: [:each | 
									RBRemoveInstanceVariableRefactoring model: model remove: each from: class]);
								yourself 
]

{ #category : 'resources' }
RBRemoveInstanceVariablesDriver >> refactoring [

	refactoring := RBRemoveInstanceVariableRefactoring model: model remove: variables first from: class
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> runRefactoring [

	self configureRefactoring.
	[
		[ 
			| changes |
			changes := refactoring generateChanges.
			self openPreviewWithChanges: changes ]
		           on: RBApplicabilityChecksFailedError
		           do: [ :err |
		           		^ RBRefactoringError signal: err messageText ] ]
		           on: RBBreakingChangeChecksFailedWarning
		           do: [ :err |
								| dialog |
								dialog := SpConfirmDialog 
									 new
										title: 'Methods still accessing variables';
										label: 'Do you want to browse them?';
										acceptLabel: 'Sure!';
										cancelLabel: 'No, forget it';
										openModal.
								dialog 
									ifTrue: [ self furtherActionFor: (RBInstanceVariableStillReferenced new refactoring: refactoring)].  err return ].
	
]

{ #category : 'initialization' }
RBRemoveInstanceVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]
