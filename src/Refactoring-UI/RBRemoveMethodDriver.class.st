"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : 'RBRemoveMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'methods',
		'senders'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RBRemoveMethodDriver >> browseSenders [

	StMessageBrowserPresenter  
		browse: (senders collect: [ :each | each value methodClass realClass >> each value selector ]) 
		asSendersOf: (refactoring refactorings flatCollect: [ :ref | ref selectors ])
	"this does not work for multiple selectors remove."
]

{ #category : 'execution' }
RBRemoveMethodDriver >> changes [

	^ refactoring generateChanges
]

{ #category : 'resources' }
RBRemoveMethodDriver >> configureRefactoring [

	| refactorings |
	refactorings := OrderedCollection new.
	(methods groupedBy: [ :m | m origin ])
		keysAndValuesDo: [ :k :v |
			refactorings add: (
				RBRemoveMethodsRefactoring
	            model: model
	            selectors: (v collect: [ :each | each selector ])
	         		from: k
				)
			].
	refactoring := RBCompositeRefactoring new
							model: model;
							refactorings: refactorings 
]

{ #category : 'execution' }
RBRemoveMethodDriver >> removeMethodChanges [ 

	refactoring refactorings do: [ :ref | ref removeMethodChanges ].
	self openPreviewWithChanges: refactoring changes

]

{ #category : 'execution' }
RBRemoveMethodDriver >> runRefactoring [

	| select |
	self configureRefactoring.
	[
	[
	senders := refactoring refactorings flatCollect: [ :ref |
		           ref senders ].

	senders ifEmpty: [ self applyChanges ] ifNotEmpty: [
		select := SpSelectDialog new
			          title: 'There are ' , senders size asString
				          , ' methods calling method(s) you want to delete';
			          label: 'Select a strategy';
			          items:
				          (RBRemoveMethodChoice subclasses collect: [ :each |
						           each new driver: self ]);
			          display: [ :each | each description ];
			          displayIcon: [ :each |
				          self iconNamed: each systemIconName ];
			          openModal.

		select ifNotNil: [ select action ] ] ]
		on: RBApplicabilityChecksFailedError
		do: [ :err | ^ RBRefactoringError signal: err messageText ] ]
		on: RBBreakingChangeChecksFailedWarning
		do: [ :err | err return ]
]

{ #category : 'initialization' }
RBRemoveMethodDriver >> scopes: refactoringScopes method: aMethod [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := { aMethod }
]

{ #category : 'initialization' }
RBRemoveMethodDriver >> scopes: refactoringScopes methods: aMethods [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := aMethods
]
