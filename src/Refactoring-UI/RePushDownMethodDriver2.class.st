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
	#name : 'RePushDownMethodDriver2',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'methods',
		'class'
	],
	#category : 'Refactoring-UI-Drivers to migrate to new architecture',
	#package : 'Refactoring-UI',
	#tag : 'Drivers to migrate to new architecture'
}

{ #category : 'initialization' }
RePushDownMethodDriver2 class >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods [

	^ self new model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods
]

{ #category : 'resources' }
RePushDownMethodDriver2 >> configureRefactoring [

	refactoring :=  RBPushDownMethodRefactoring
		  model: model
		  pushDown: (methods collect: [ :each | each selector ])
		  from: class
]

{ #category : 'initialization' }
RePushDownMethodDriver2 >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methodsList [
	"Push down a list of methods of the same class"
	
	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'execution' }
RePushDownMethodDriver2 >> runRefactoring [

	| changes |
	methods := self selectMethods.
	"the user can still select if needed. Nil = cancel refactoring"
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
RePushDownMethodDriver2 >> selectMethods [

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
