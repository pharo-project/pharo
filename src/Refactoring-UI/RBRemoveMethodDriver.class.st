"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : #RBRemoveMethodDriver,
	#superclass : #RBDriver,
	#instVars : [
		'class',
		'variables',
		'refactoring',
		'methods'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #resources }
RBRemoveMethodDriver >> refactoring [

	refactoring := RBRemoveMethodRefactoring model: model removeMethods: methods from: class
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
										title: 'Methods are still calling removed method';
										label: 'Do you want to browse them?';
										acceptLabel: 'Sure!';
										cancelLabel: 'No, forget it';
										openModal.
								dialog 
									ifTrue: [ self furtherActionFor: (RBMethodStillCalled new refactoring: refactoring)].  err return ].
	
]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes method: aMethod for: aClass [
	
	scopes := refactoringScopes.
	variables := {aMethod}.
	class := aClass
]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes methods: aMethods for: aClass [
	
	scopes := refactoringScopes.
	methods := aMethods.
	class := aClass
]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes variable: aVariable for: aClass [
	
	scopes := refactoringScopes.
	variables := {aVariable}.
	class := aClass
]
