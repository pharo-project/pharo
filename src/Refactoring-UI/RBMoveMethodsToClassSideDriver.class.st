"
I represent a driver that invokes `MoveToClassSide` refactoring.

I implement methods from parent class and define some helper methods that are needed to execute `MoveToClassSide` refactoring:
- `generateRefactorings` - creating `MoveToClassSide` refactorings for each of the selected methods
- checking preconditions and breaking changes
- executing the refactoring

You can create my instance and execute the refactoring by running:

```
(RBMoveMethodsToClassSideDriver model: aRBNamespace methods: aCollectionOfMethods) runRefactoring
```
"
Class {
	#name : 'RBMoveMethodsToClassSideDriver',
	#superclass : 'ReInteractionDriver',
	#category : 'Refactoring-UI-Drivers to migrate to new architecture',
	#package : 'Refactoring-UI',
	#tag : 'Drivers to migrate to new architecture'
}

{ #category : 'instance creation' }
RBMoveMethodsToClassSideDriver class >> scopes: scopesList methods: aCollectionOfMethods [

	^ self new scopes: scopesList methods: aCollectionOfMethods
]

{ #category : 'execution' }
RBMoveMethodsToClassSideDriver >> runRefactoring [

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
					          title: 'Method already defined in the class side';
					          label: 'Are you sure you want to override it?';
					          acceptLabel: 'Sure!';
					          cancelLabel: 'No, forget it';
					          openModal.
				dialog ifTrue: [
					self furtherActionFor:
						(ReOverrideMethod new refactoring: refactoring) ].
				err return ]
]

{ #category : 'initialization' }
RBMoveMethodsToClassSideDriver >> scopes: scopesList methods: methods [

	scopes := scopesList.
	model := self refactoringScopeOn: scopesList first.
	refactoring := ReCompositeRefactoring new
		               model: model;
		               refactorings: (methods collect: [ :m |
					                RBMoveMethodToClassSideRefactoring
						                model: model
						                method: m
						                class: m origin ])
]
