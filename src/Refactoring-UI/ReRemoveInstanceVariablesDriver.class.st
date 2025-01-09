"
I'm a model for user interaction for the remove instance variable refactoring.
"
Class {
	#name : 'ReRemoveInstanceVariablesDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> breakingChoices [

	| items |
	items := OrderedCollection new.
	items add: (ReBrowseReferencedInstanceVariableChoice new driver: self; yourself).
	items add: (ReRemoveInstanceVariableChoice new driver: self; yourself).
	items add: (ReRemoveInstanceVariableAndShowChoice new driver: self;  yourself).
	^ items
]

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> browseInstanceVariableReferences [

	StMessageBrowser
		browse: refactoring violators
		
]

{ #category : 'resources' }
ReRemoveInstanceVariablesDriver >> configureRefactoring [

	refactoring := ReUpFrontPreconditionCheckingCompositeRefactoring new
							model: model; 
							refactorings: (variables collect: [:each | 
									ReRemoveInstanceVariableRefactoring model: model remove: each from: class]);
								yourself.
	refactoring prepareForInteractiveMode
]

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> defaultSelectDialog [

	^ SpSelectDialog new
		  title: 'There are references to variables!';
		  items: self breakingChoices;
		  display: [ :each | each description ];
		  displayIcon: [ :each | self iconNamed: each systemIconName ];
		  openModal
]

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> handleBreakingChanges [

	| select |
	select := self selectDialog.
	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> runRefactoring [

	self configureRefactoring.
	
	"Normally from the IDE we should not get bogus variables."
	"we could call the violatorMessages here to let the applicability preconditions do the job."
	refactoring failedApplicabilityPreconditions 
		ifNotEmpty: [ ^ self inform: 'The variable ', refactoring variable,  ' does not exist' ].
	
	(refactoring breakingChangePreconditions allSatisfy: [:each | each check])
		ifTrue: [ self applyChanges ]
		ifFalse: [ self handleBreakingChanges ]

]

{ #category : 'initialization' }
ReRemoveInstanceVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]

{ #category : 'execution' }
ReRemoveInstanceVariablesDriver >> selectDialog [
	
	^ selectDialog ifNil: [ selectDialog := self defaultSelectDialog ].
	
]
