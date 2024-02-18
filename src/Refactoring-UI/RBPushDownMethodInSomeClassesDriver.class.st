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
	#superclass : 'RePushDownMethodDriver',
	#instVars : [
		'classes'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
RBPushDownMethodInSomeClassesDriver >> configureRefactoring [

	refactoring :=  RBPushDownMethodRefactoring
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: class
		in: classes.
]

{ #category : 'execution' }
RBPushDownMethodInSomeClassesDriver >> gatherUserInput [

	super gatherUserInput.
	classes := self selectClasses
]

{ #category : 'initialization' }
RBPushDownMethodInSomeClassesDriver >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methodsList [

	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
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

	self gatherUserInput.
	methods ifNil: [ ^ self ].
	classes ifNil: [ ^ self ].
	
	self configureAndRunRefactoring 
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
	^ dialog selectedItems collect: [ :each | each name ]
]
