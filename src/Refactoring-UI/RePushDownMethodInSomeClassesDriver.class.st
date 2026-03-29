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

	super gatherUserInput.
	classes := self selectClasses
]

{ #category : 'configuration' }
RePushDownMethodInSomeClassesDriver >> instantiateRefactoring [

	^ RePushDownMethodRefactoring
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: class
		in: classes.
]

{ #category : 'initialization' }
RePushDownMethodInSomeClassesDriver >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methodsList [

	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : 'resources' }
RePushDownMethodInSomeClassesDriver >> refactoring [

	refactoring :=  RePushDownMethodRefactoring
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: class
		in: classes.
]

{ #category : 'execution' }
RePushDownMethodInSomeClassesDriver >> runRefactoring [

	self gatherUserInput.
	methods ifNil: [ ^ self ].
	classes ifNil: [ ^ self ].
	self configureAndRunRefactoring 
]

{ #category : 'execution' }
RePushDownMethodInSomeClassesDriver >> selectClasses [
	| dialog |
	
	dialog := (StVariablesSelectionPresenter newApplication: self application)
		label: 'Push down in classes: '
			withItems:(class subclasses sort: #name ascending) asOrderedCollection
			selecting: class subclasses;
		openDialog.
	
	dialog isOk ifFalse: [ ^ nil ].
	
	^ dialog selectedItems collect: [ :each | each name ]
]
