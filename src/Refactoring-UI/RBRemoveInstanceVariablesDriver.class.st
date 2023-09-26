"
I'm a model for user interaction for the remove instance variable refactoring.
"
Class {
	#name : 'RBRemoveInstanceVariablesDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'class',
		'variables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'as yet unclassified' }
RBRemoveInstanceVariablesDriver >> browseInstanceVariableReferences [

	StMessageBrowserPresenter
		browse: refactoring violators
		
]

{ #category : 'resources' }
RBRemoveInstanceVariablesDriver >> configureRefactoring [

	refactoring := RBCompositeRefactoring2 new
							model: model; 
							refactorings: (variables collect: [:each | 
									RBRemoveInstanceVariableRefactoring model: model remove: each from: class]);
								yourself.
	refactoring prepareForInteractiveMode
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> handleBreakingChanges [

	| select items |
	items := OrderedCollection new.
	items add: (RBBrowseReferencedInstanceVariableChoice new
						driver: self ; yourself).
	items add: (RBRemoveInstanceVariableChoice new driver: self; yourself).
	items add: (RBRemoveInstanceVariableAndShowChoice new driver: self; yourself).
	
	select := SpSelectDialog new
		          title: 'There are references to variables!';
		          label: 'self labelBasedOnBreakingChanges';
		          items: items;
		          display: [ :each | each description ];
		          displayIcon: [ :each |  self iconNamed: each systemIconName ];
		          openModal.

	select ifNotNil: [ select action ]
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> removeInstanceVariables [

	| changes |
	changes := refactoring privateTransform.
	self openPreviewWithChanges: changes
]

{ #category : 'execution' }
RBRemoveInstanceVariablesDriver >> runRefactoring [

	self configureRefactoring.
	
	refactoring applicabilityPreconditions check ifFalse: [
		RBRefactoringError signal: refactoring applicabilityPreconditions errorString ].
	refactoring breakingChangePreconditions check
		ifTrue: [ self removeInstanceVariables ]
		ifFalse: [ self handleBreakingChanges ]

	"[
		[ 
			| changes |
			changes := refactoring generateChanges.
			self openPreviewWithChanges: changes ]
		           on: RBApplicabilityChecksFailedError
		           do: [ :err |
		           		^ RBRefactoringError signal: err messageText ] ]
		           on: RBBreakingChangeChecksFailedWarning
		           do: [ :err |
								| dialog |
								dialog := SpConfirmDialog 
									 new
										title: 'Methods still accessing variables';
										label: 'Do you want to browse them?';
										acceptLabel: 'Sure!';
										cancelLabel: 'No, forget it';
										openModal.
								dialog 
									ifTrue: [ self furtherActionFor: (RBInstanceVariableStillReferenced new refactoring: refactoring)].  err return ]."
	
]

{ #category : 'initialization' }
RBRemoveInstanceVariablesDriver >> scopes: refactoringScopes variables: aCollection for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	variables := aCollection.
	class := aClass
]
