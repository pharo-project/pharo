"
I represent a driver that invokes `PushDownMethod` refactoring.

I am responsible for asking user which methods to push down and to which classes to push to.

When I gather all needed information I am calling and executing push down method refactoring.

You can create my instance and execute the refactoring by running:

```
(RBPushDownMethodDriver model: aRBNamespace scopes: refactoringScopes pushDown: methods) execute
```
"
Class {
	#name : #RBPushDownMethodInSomeClassesDriver,
	#superclass : #RBDriver,
	#instVars : [
		'methods',
		'scopes',
		'class',
		'classes'
	],
	#category : #'Refactoring-UI'
}

{ #category : #initialization }
RBPushDownMethodInSomeClassesDriver class >> model: aRBNamespace scopes: refactoringScopes pushDown: methods [

	^ self new model: aRBNamespace scopes: refactoringScopes pushDown: methods
]

{ #category : #execution }
RBPushDownMethodInSomeClassesDriver >> execute [

	| changes refactoring |
	methods := self selectMethods.
	methods ifNil: [ ^ self ].
	classes := self selectClasses.
	classes ifNil: [ ^ self ].
	
	refactoring := self refactoring.
	changes := [
	           [ refactoring generateChanges ]
		           on: RBApplicabilityChecksFailedError
		           do: [ :err |
		           ^ RBRefactoringFailure signal: err messageText ] ]
		           on: RBBreakingChangeChecksFailedWarning
		           do: [ :err |
			           RBRefactoringWarning signal: err messageText.
			           "If user answers no, error is being propagated."
			           err resume ].
	(StRefactoringPreviewPresenter
		 changes: changes
		 inEnvironment: model
		 scopes: scopes) open
]

{ #category : #initialization }
RBPushDownMethodInSomeClassesDriver >> model: aRBNamespace scopes: refactoringScopes pushDown: methodsList [

	model := aRBNamespace.
	scopes := refactoringScopes.
	methods := methodsList.
	class := methods first origin
]

{ #category : #resources }
RBPushDownMethodInSomeClassesDriver >> refactoring [

	^ RBPushDownMethodRefactoring
		model: model
		pushDown: (methods collect: [ :each | each selector ])
		from: class
		in: classes.
]

{ #category : #execution }
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

{ #category : #execution }
RBPushDownMethodInSomeClassesDriver >> selectMethods [

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
