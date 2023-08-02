Class {
	#name : #RBRemoveMethodDriver,
	#superclass : #RBDriver,
	#instVars : [
		'class',
		'variables',
		'refactoring'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #resources }
RBRemoveMethodDriver >> refactoring [

	refactoring := RBRemoveInstanceVariableRefactoring model: model remove: variables first from: class
]

{ #category : #execution }
RBRemoveMethodDriver >> runRefactoring [

	self refactoring.
	[
		[ 
			| changes |
			changes := refactoring generateChanges.
			self openPreviewWithChanges: { changes } ]
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

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes variable: aVariable for: aClass [
	
	scopes := refactoringScopes.
	variables := {aVariable}.
	class := aClass
]
