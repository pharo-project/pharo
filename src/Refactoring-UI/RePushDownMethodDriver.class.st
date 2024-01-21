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
	#name : 'RePushDownMethodDriver',
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
RePushDownMethodDriver class >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods [

	^ self new model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods
]

{ #category : 'execution' }
RePushDownMethodDriver >> changes [
	"it should NOT be generate changes."
	^  refactoring privateTransform

]

{ #category : 'resources' }
RePushDownMethodDriver >> configureRefactoring [

	refactoring :=  RBPushDownMethodRefactoring
		  model: model
		  pushDown: (methods collect: [ :each | each selector ])
		  from: class
]

{ #category : 'configuration' }
RePushDownMethodDriver >> defaultSelectDialog [

	^ SpSelectDialog new
		    title: 'There are potential breaking changes!';
		          label: #('to be done' 'yes');
		          items: #('to be done' 'yes');
		          display: [ :each | each " description" ];
		          displayIcon: [ :each | self iconNamed: each systemIconName ];
		          openModal
]

{ #category : 'execution' }
RePushDownMethodDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'initialization' }
RePushDownMethodDriver >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methodsList [
	"Push down a list of methods of the same class"
	
	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'execution' }
RePushDownMethodDriver >> runRefactoring [

	methods := self selectMethods.
	"the user can still select if needed. Nil = cancel refactoring"
	methods ifNil: [ ^ self ].
	self configureRefactoring.

	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ ^ self inform: 'The shared variable does not exist' ].

	refactoring failedBreakingChangePreconditions
		ifEmpty: [ self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]
]

{ #category : 'execution' }
RePushDownMethodDriver >> selectMethods [

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
