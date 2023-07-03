Class {
	#name : #RBRemoveInstanceVariableDriver,
	#superclass : #RBDriver,
	#instVars : [
		'class',
		'variables',
		'refactoring'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #execution }
RBRemoveInstanceVariableDriver >> execute [

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

{ #category : #execution }
RBRemoveInstanceVariableDriver >> furtherActionFor: aReport [
	
	"Smalltalk tools messageList
		new
		  messages: (self halt);
		  title: 'jlklkjljlkjkl';
		  open."
	
	"for now use the report to see and not the driver state. To be reevaluated later."
	"use system navigation to avoid to duplicate logic. 
	Now it could be packaged in the report object.
	Note that the report holds an environment and that we also have scopes so the message
	list should be passed the scope when the widget will be able to handle it."
	aReport execute.


]

{ #category : #resources }
RBRemoveInstanceVariableDriver >> refactoring [

	refactoring := RBRemoveInstanceVariableRefactoring2 
		  model: model remove: variables first from: class
]

{ #category : #initialization }
RBRemoveInstanceVariableDriver >> scopes: refactoringScopes variable: aVariable for: aClass [
	
	scopes := refactoringScopes.
	variables := {aVariable}.
	class := aClass
]
