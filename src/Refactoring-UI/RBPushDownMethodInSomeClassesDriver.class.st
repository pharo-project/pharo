"
I represent a driver that invokes `PushDownMethod` refactoring.

I am responsible for asking user which methods to push down and to which classes to push to.

When I gather all needed information I am calling and executing push down method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBPushDownMethodDriver model: aRBNamespace scopes: refactoringScopes pushDown: methods) runRefactoring
```
"
Class {
	#name : 'RBPushDownMethodInSomeClassesDriver',
	#superclass : 'RBPushDownMethodDriver',
	#instVars : [
		'classes'
	],
	#category : 'Refactoring-UI-Drivers to migrate to new architecture',
	#package : 'Refactoring-UI',
	#tag : 'Drivers to migrate to new architecture'
}

{ #category : 'resources' }
RBPushDownMethodInSomeClassesDriver >> configureRefactoring [

	refactoring :=  RBPushDownMethodRefactoring
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: class
		in: classes.
]

{ #category : 'resources' }
RBPushDownMethodInSomeClassesDriver >> refactoring [

	refactoring :=  RBPushDownMethodRefactoring
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: class
		in: classes.
]

{ #category : 'execution' }
RBPushDownMethodInSomeClassesDriver >> runRefactoring [

	| changes  |
	methods := self selectMethods.
	methods ifNil: [ ^ self ].
	classes := self selectClasses.
	classes ifNil: [ ^ self ].
	
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
RBPushDownMethodInSomeClassesDriver >> selectClasses [

	| dialog |
	dialog := StVariablesSelectionPresenter
		          label: 'Push down in classes: '
		          withItems:
			          (class subclasses sort: [ :a :b | a name < b name ])
				          asOrderedCollection
		          selecting: class subclasses.
	dialog cancelled ifTrue: [ ^ nil ].
	^ dialog presenter selectedItems collect: [ :each | each name ]
]
