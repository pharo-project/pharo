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
		'isAbstract',
		'subclassDirectlyDefines',
		'refersToSelector',
		'allSubclassesDirectlyDefines'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
RePushDownMethodDriver class >> driverName [

	^ 'Push down method(s)'
]

{ #category : 'initialization' }
RePushDownMethodDriver class >> model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods [

	^ self new model: aRBNamespace scopes: refactoringScopes pushDownMethods: methods
]

{ #category : 'execution' }
RePushDownMethodDriver >> breakingChoices [

	| choices |
	choices := OrderedCollection new.
	choices add: (RePushDownMethodTransformationChoice new driver: self).
	subclassesSendSuper isFalse ifTrue: [ choices add: (ReBrowseSubclassSuperSendsChoice new driver: self) ].
	refersToSelector isFalse ifTrue: [ choices add: (ReBrowseMethodsChoice new driver: self) ].
	allSubclassesDirectlyDefines isFalse ifTrue: [ choices add: (ReRemoveSuperclassMethodChoice new driver: self) ].
	^ choices
]

{ #category : 'actions' }
RePushDownMethodDriver >> browseMethods [

	| violators |
	violators := refersToSelector violators.
	(application toolNamed: #messageList) browse: (violators flatCollect: [ :v | v collect: #compiledMethod ])
]

{ #category : 'actions' }
RePushDownMethodDriver >> browseSubclassesSuperSends [

	subclassesSendSuper violators keysAndValuesDo: [ :violator :references | 
			(application toolNamed: #messageList)
				browse: (references collect: [ :ref | ref compiledMethod ])
				asSendersOf: violator realClass ]
]

{ #category : 'execution' }
RePushDownMethodDriver >> changes [
	"it should NOT be generate changes."
	^  refactoring privateTransform

]

{ #category : 'configuration' }
RePushDownMethodDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'execution' }
RePushDownMethodDriver >> gatherUserInput [

	self selectMethods.
	methods ifNil: [ ^ false ].
	refactoring selectors: (methods collect: [ :each | each selector ]).
	^ true
]

{ #category : 'private exe' }
RePushDownMethodDriver >> hasFailedBehaviorPreservingPreconditions [

	isAbstract := refactoring preconditionIsAbstract.
	subclassesSendSuper := refactoring preconditionSubclassesDontSendSuper.
	subclassDirectlyDefines := refactoring preconditionSubclassesDirectlyDefines.
	refersToSelector := refactoring preconditionMethodsReferToSelector.
	allSubclassesDirectlyDefines := refactoring preconditionAllSubclassesDirectlyDefines.
	
	^ refactoring failedBreakingChangePreconditions isNotEmpty 
]

{ #category : 'configuration' }
RePushDownMethodDriver >> instantiateRefactoring [

	^ RePushDownMethodRefactoring
		  model: model
		  pushDown: #()
		  from: class name
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
			  subclassDirectlyDefines isFalse ifTrue: [
					  subclassDirectlyDefines violationMessageOn: stream.
					  stream cr ].
			  refersToSelector isFalse ifTrue: [
					  refersToSelector violationMessageOn: stream.
					  stream cr ].
			  allSubclassesDirectlyDefines isFalse ifTrue: [
					  allSubclassesDirectlyDefines violationMessageOn: stream.
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

{ #category : 'removing' }
RePushDownMethodDriver >> removeSuperclassMethod [

	| selectorsToRemove |
	selectorsToRemove := allSubclassesDirectlyDefines violators values flatCollect: [ :v | v collect: #selector ].
	selectorsToRemove := selectorsToRemove asSet.
	selectorsToRemove do: [ :each | refactoring generateChangesFor: (RBRemoveMethodTransformation selector: each from: class) ].
	self openPreviewWithChanges: refactoring changes
]

{ #category : 'execution' }
RePushDownMethodDriver >> selectMethods [

	| dialog |
	
	dialog := (self methodsSelectionPresenterClass newApplication: self application)
		          title: 'There are potential breaking changes!'
		          label: 'Push down methods from ' , class name
		          withItems: (class methods sort: [ :a :b | a asString < b asString ]) asOrderedCollection
		          selecting: methods asOrderedCollection
		          acceptBlock: [ :selectedMethods | methods := selectedMethods ];
		          openModal.

	dialog isOk ifTrue: [ ^ self ].

	methods:= nil
]
