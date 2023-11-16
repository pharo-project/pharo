"
I represent a driver that invokes `PushDownMethod` refactoring.

I am responsible for asking user which methods to push down.

When I gather all needed information I am calling and executing push down method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBPushDownMethodDriver model: aRBNamespace scopes: refactoringScopes pushDown: methods) runRefactoring
```
"
Class {
	#name : 'RBPushDownMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'methods',
		'class'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'initialization' }
RBPushDownMethodDriver class >> model: aRBNamespace scopes: refactoringScopes pushDown: methods [

	^ self new model: aRBNamespace scopes: refactoringScopes pushDown: methods
]

{ #category : 'resources' }
RBPushDownMethodDriver >> configureRefactoring [

	refactoring :=  RBPushDownMethodRefactoring
		  model: model
		  pushDown: (methods collect: [ :each | each selector ])
		  from: class
]

{ #category : 'initialization' }
RBPushDownMethodDriver >> model: aRBNamespace scopes: refactoringScopes pushDown: methodsList [

	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'resources' }
RBPushDownMethodDriver >> refactoring [

	refactoring :=  RBPushDownMethodRefactoring
		  model: model
		  pushDown: (methods collect: [ :each | each selector ])
		  from: class
]

{ #category : 'execution' }
RBPushDownMethodDriver >> runRefactoring [

	| changes |
	methods := self selectMethods.
	methods ifNil: [ ^ self ].

	self configureRefactoring.
	changes := [
	           [ refactoring generateChanges ]
		           on: RBApplicabilityChecksFailedError
		           do: [ :err |
		           ^ RBRefactoringError signal: err messageText ] ]
		           on: RBBreakingChangeChecksFailedWarning
		           do: [ :err |
			           RBRefactoringWarning signal: err messageText.
			           "If user answers no, error is being propagated."
			           err resume ].
	self openPreviewWithChanges: changes
]

{ #category : 'execution' }
RBPushDownMethodDriver >> selectMethods [

	| dialog |
	dialog := StMethodsSelectionPresenter
		          label: 'Push down methods from ' , class name
		          withItems:
			          (class methods sort: [ :a :b | a asString < b asString ])
				          asOrderedCollection
		          selecting: methods asOrderedCollection.
	dialog cancelled ifTrue: [ ^ nil ].
	^ dialog presenter selectedItems
]