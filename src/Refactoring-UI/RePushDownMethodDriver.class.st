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
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'methods',
		'class',
		'methodSelectionPresenterClass',
		'subclassesSendSuper',
		'isAbstract'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'initialization' }
RePushDownMethodDriver class >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods [

	^ self new model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods
]

{ #category : 'execution' }
RePushDownMethodDriver >> breakingChoices [

	| choices |
	choices := OrderedCollection new.
	choices add: (RePushDownMethodChoice new driver: self).
	subclassesSendSuper isFalse ifTrue: [
		choices add: (ReBrowseSubclassSuperSendsChoice new driver: self) ].
	^ choices 
]

{ #category : 'actions' }
RePushDownMethodDriver >> browseSubclassesSuperSends [

	subclassesSendSuper violators keysAndValuesDo: [ :violator :references |
		"we should pass the application! "
		StMessageBrowser
			browse: (references collect: [ :ref | ref compiledMethod ])
			asSendersOf: violator realClass ]
]

{ #category : 'execution' }
RePushDownMethodDriver >> changes [
	"it should NOT be generate changes."
	^  refactoring privateTransform

]

{ #category : 'execution' }
RePushDownMethodDriver >> configureAndRunRefactoring [

	self configureRefactoring.

	refactoring failedApplicabilityPreconditions ifNotEmpty: [
		^ self inform: 'The shared variable does not exist' ].
	self setBreakingChangesPreconditions.
	refactoring failedBreakingChangePreconditions
		ifEmpty: [ self applyChanges ]
		ifNotEmpty: [ self handleBreakingChanges ]
]

{ #category : 'resources' }
RePushDownMethodDriver >> configureRefactoring [

	refactoring :=  RBPushDownMethodRefactoring
		  model: model
		  pushDown: (methods collect: [ :each | each selector ])
		  from: class name
]

{ #category : 'configuration' }
RePushDownMethodDriver >> defaultSelectDialog [

	^ SpSelectDialog new
		    title: 'There are potential breaking changes!';
		          label: self labelBasedOnBreakingChanges;
		          items: self breakingChoices;
		          display: [ :each | each description ];
		          displayIcon: [ :each | self iconNamed: each systemIconName ];
		          openModal
]

{ #category : 'execution' }
RePushDownMethodDriver >> gatherUserInput [

	methods := self selectMethods
]

{ #category : 'execution' }
RePushDownMethodDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'ui - dialogs' }
RePushDownMethodDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
		  isAbstract isFalse ifTrue: [
			  isAbstract violationMessageOn: stream.
			  stream cr ].
		  subclassesSendSuper isFalse ifTrue: [
			  subclassesSendSuper violationMessageOn: stream.
			  stream cr ].
		  stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'for mocks' }
RePushDownMethodDriver >> methodsSelectionPresenterClass [

	^ methodSelectionPresenterClass ifNil: [ methodSelectionPresenterClass := StMethodsSelectionPresenter ]
]

{ #category : 'for mocks' }
RePushDownMethodDriver >> methodsSelectionPresenterClass: aClass [ 

	methodSelectionPresenterClass := aClass
]

{ #category : 'initialization' }
RePushDownMethodDriver >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methodsList [
	"Push down a list of methods of the same class"
	
	scopes := refactoringScopes.
	model := aRBNamespace ifNil: [ self refactoringScopeOn: scopes first ].
	methods := methodsList.
	class := methods first origin
]

{ #category : 'execution' }
RePushDownMethodDriver >> pushDownMethods [

	self openPreviewWithChanges: refactoring pushDownMethodChanges 
]

{ #category : 'execution' }
RePushDownMethodDriver >> runRefactoring [

	"the user can still select if needed. Nil = cancel refactoring"
	self gatherUserInput.
	methods ifNil: [ ^ self ].
	self configureAndRunRefactoring
]

{ #category : 'execution' }
RePushDownMethodDriver >> selectMethods [

	| dialog |
	dialog := self methodsSelectionPresenterClass 
		          label: 'Push down methods from ' , class name
		          withItems:
			          (class methods sort: [ :a :b | a asString < b asString ])
				          asOrderedCollection
		          selecting: methods asOrderedCollection.
	dialog cancelled ifTrue: [ ^ nil ].
	^ dialog selectedItems
]

{ #category : 'initialization' }
RePushDownMethodDriver >> setBreakingChangesPreconditions [ 

	isAbstract := refactoring preconditionIsAbstract.
	subclassesSendSuper := refactoring preconditionSubclassesDontSendSuper
]
