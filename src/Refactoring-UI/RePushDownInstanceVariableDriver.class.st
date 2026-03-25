"
I represent a driver that invokes `PushDownInstanceVariables` refactoring.

I am responsible for asking user which variables to push down.

"
Class {
	#name : 'RePushDownInstanceVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'variables',
		'methodSelectionPresenterClass',
		'referencesInstanceVariableCollection'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
RePushDownInstanceVariableDriver class >> driverName [

	^ 'Push down instance variable(s)'
]

{ #category : 'execution' }
RePushDownInstanceVariableDriver >> breakingChoices [

	| choices |
	choices := OrderedCollection new.
	choices add: (RePushDownMethodTransformationChoice new driver: self).
	(referencesInstanceVariableCollection anySatisfy: [ :precond | precond isFalse ]) ifTrue: [
		choices add: (ReBrowseMethodsChoice new driver: self) ].


	^ choices
]

{ #category : 'actions' }
RePushDownInstanceVariableDriver >> browseMethods [

	| violators |
	violators := referencesInstanceVariableCollection flatCollect: [ :c |
		             (c violators collect: [ :method | variables first definingClass compiledMethodAt: method ]) asSet ].


	(application toolNamed: #messageList) browse: violators
]

{ #category : 'execution' }
RePushDownInstanceVariableDriver >> configureAndRunRefactoring [

	self configureRefactoring.
	self setBreakingChangesPreconditions.
	refactoring refactorings do: [ :r |
		r failedBreakingChangePreconditions ifEmpty: [ self applyChanges ] ifNotEmpty: [ self handleBreakingChanges ] ]
]

{ #category : 'configuration' }
RePushDownInstanceVariableDriver >> defaultSelectDialog [

	^ super defaultSelectDialog
		  title: 'There are potential breaking changes!';
		  message: self labelBasedOnBreakingChanges;
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  yourself
]

{ #category : 'execution' }
RePushDownInstanceVariableDriver >> gatherUserInput [

	variables := self selectVariables.
]

{ #category : 'configuration' }
RePushDownInstanceVariableDriver >> instantiateRefactoring [

	^ ReCompositeRefactoring new
		  model: model;
		  refactorings: (variables collect: [ :v |
					   RBPushDownInstanceVariableRefactoring model: model variable: v name class: variables first definingClass ])
]

{ #category : 'ui - dialogs' }
RePushDownInstanceVariableDriver >> labelBasedOnBreakingChanges [

	^ String streamContents: [ :stream |
			  (referencesInstanceVariableCollection anySatisfy: [ :precond | precond isFalse ]) ifTrue: [
					  referencesInstanceVariableCollection first violationMessageOn: stream.
					  stream cr ].
			  stream nextPutAll: 'Select a strategy' ]
]

{ #category : 'execution' }
RePushDownInstanceVariableDriver >> runRefactoring [
	"the user can still select if needed. Nil = cancel refactoring"

	self gatherUserInput.
	variables ifNil: [ ^ self ].
	self configureAndRunRefactoring
]

{ #category : 'accessing' }
RePushDownInstanceVariableDriver >> scopes: scopesList [ 

	scopes := scopesList.
	model := self refactoringScopeOn: scopesList first.

]

{ #category : 'execution' }
RePushDownInstanceVariableDriver >> selectVariables [

	| dialog |
	dialog := (self variablesSelectionPresenterClass newApplication: self application)
		          title: 'There are potential breaking changes!'
		          label: 'Push down variables from ' , variables first definingClass name
		          withItems: (variables sort: [ :a :b | a asString < b asString ]) asOrderedCollection
		          selecting: variables asOrderedCollection;
		          openModal.

	dialog isOk ifFalse: [ ^ nil ].

	^ dialog presenter selectedItems
]

{ #category : 'initialization' }
RePushDownInstanceVariableDriver >> setBreakingChangesPreconditions [

	referencesInstanceVariableCollection := refactoring refactorings collect: [ :r | r preconditionReferencesInstanceVariable ]
]

{ #category : 'accessing' }
RePushDownInstanceVariableDriver >> variables: aCollectionOfVariables [

	variables := aCollectionOfVariables
]

{ #category : 'for mocks' }
RePushDownInstanceVariableDriver >> variablesSelectionPresenterClass [

	^ methodSelectionPresenterClass ifNil: [ methodSelectionPresenterClass := StVariablesSelectionPresenter ]
]

{ #category : 'for mocks' }
RePushDownInstanceVariableDriver >> variablesSelectionPresenterClass: aClass [

	methodSelectionPresenterClass := aClass
]
