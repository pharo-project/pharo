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
	#name : 'RePushDownMethodInSomeClassesDriver',
	#superclass : 'RePushDownMethodDriver',
	#instVars : [
		'classes'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
RePushDownMethodInSomeClassesDriver class >> driverName [

	^ 'Push down method(s) in some classes'
]

{ #category : 'execution' }
RePushDownMethodInSomeClassesDriver >> gatherUserInput [

	self selectClasses.
	classes ifNil: [ ^ false ].
	refactoring classes: (classes collect: [ :each | each name ]).
	^ true
]

{ #category : 'configuration' }
RePushDownMethodInSomeClassesDriver >> instantiateRefactoring [

	^ RePushDownMethodRefactoring
		model: model 
		pushDown: (methods collect: [ :each | each selector ])
		from: class
		in: #().
]

{ #category : 'initialization' }
RePushDownMethodInSomeClassesDriver >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methodsList [

	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'execution' }
RePushDownMethodInSomeClassesDriver >> selectClasses [

	| dialog |
	dialog := (StVariablesSelectionPresenter newApplication: self application)
		          title: 'Push down in some classes'
		          label: 'Push down in classes: '
		          withItems: (class subclasses sort: #name ascending) asOrderedCollection
		          selecting: class subclasses asOrderedCollection
		          acceptBlock: [ :selectedClasses | classes := selectedClasses ];
		          openModal.
	dialog isOk ifTrue: [ ^ self ].
	classes := nil
]
