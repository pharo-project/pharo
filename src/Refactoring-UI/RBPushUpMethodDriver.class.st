"
I represent a driver that invokes `PushUpMethod` refactoring.

I am responsible for asking user which methods to push up and to which super class to push it to.
When I gather all needed information I am calling and executing push up method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBPushUpMethodDriver model: aRBNamespace scopes: refactoringScopes pushUp: methods) runRefactoring
```
"
Class {
	#name : 'RBPushUpMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'class',
		'methods',
		'superclass'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'initialization' }
RBPushUpMethodDriver class >> model: aRBNamespace scopes: refactoringScopes pushUp: methodsList [

	^ self new
		  model: aRBNamespace
		  scopes: refactoringScopes
		  pushUp: methodsList
]

{ #category : 'resources' }
RBPushUpMethodDriver >> configureRefactoring [
	"I'm gathering user input and creating refactoring instance"

	self selectMethodsAndSuperclass.
	refactoring := RBPullUpMethodRefactoring
		  model: model
		  pullUp: (methods collect: [ :each | each selector ])
		  from: methods first origin
		  to: superclass
]

{ #category : 'initialization' }
RBPushUpMethodDriver >> model: aRBNamespace scopes: refactoringScopes pushUp: methodsList [

	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'resources' }
RBPushUpMethodDriver >> refactoring [
	"I'm gathering user input and creating refactoring instance"

	self selectMethodsAndSuperclass.
	refactoring := RBPullUpMethodRefactoring
		  model: model
		  pullUp: (methods collect: [ :each | each selector ])
		  from: methods first origin
		  to: superclass
]

{ #category : 'execution' }
RBPushUpMethodDriver >> runRefactoring [

	| changes |
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

{ #category : 'initialization - data' }
RBPushUpMethodDriver >> selectMethodsAndSuperclass [

	| classes |
	classes := class allSuperclasses removeAllSuchThat: [ :each |
		           each == Object or: [ each == ProtoObject ] ].
	StSelectClassAndMethodsPresenter
		label: 'Methods to be pull up'
		dropLabel: 'Pull up methods of ' , class name , ' to:'
		withItems:
			(class methods sort: [ :a :b | a asString < b asString ])
				asOrderedCollection
		selecting: methods asOrderedCollection
		dropItems: classes
		acceptBlock: [ :item :items |
			superclass := item.
			methods := items ]
]
