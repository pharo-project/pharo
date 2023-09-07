"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : #RBRemoveMethodDriver,
	#superclass : #RBDriver,
	#instVars : [
		'class',
		'refactoring',
		'methods',
		'senders'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #execution }
RBRemoveMethodDriver >> browseSenders [

	StMessageBrowserPresenter  
		browse: (senders collect: [ :each | each value methodClass realClass >> each value selector ]) 
		asSendersOf: refactoring selectors
	"this does not work for multiple selectors remove."
]

{ #category : #resources }
RBRemoveMethodDriver >> configureRefactoring [

	refactoring := RBRemoveMethodsRefactoring
		               model: model
		               selectors: (methods collect: [ :each | each selector ])
		               from: class
]

{ #category : #accessing }
RBRemoveMethodDriver >> refactoring [

	^ refactoring
]

{ #category : #execution }
RBRemoveMethodDriver >> removeMethodChanges [ 

	self openPreviewWithChanges: refactoring removeMethodChanges

]

{ #category : #execution }
RBRemoveMethodDriver >> runRefactoring [

	| select |
	self configureRefactoring.
	[
	[
	senders := refactoring senders.

	senders isEmpty
		ifTrue: [ self openPreviewWithChanges: refactoring generateChanges ]
		ifFalse: [ 
		
		select := SpSelectDialog new 
			title: 'There are ', senders size asString,  ' methods calling method(s) you want to delete';
			label: 'Select a strategy';
			items: (RBRemoveMethodChoice subclasses collect: [ :each | each  new driver: self]);
			display: [ :each | each description ];
			displayIcon: [ :each | self iconNamed: each systemIconName ];
			openModal.
			
	select 
		ifNotNil: [ 
			select action ]]]
	
		on: RBApplicabilityChecksFailedError
		do: [ :err | ^ RBRefactoringError signal: err messageText ] ]
		on: RBBreakingChangeChecksFailedWarning
		do: [ :err |	 err return ]
	

]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes method: aMethod for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := {aMethod}.
	class := aClass
]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes methods: aMethods for: aClass [
	
	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes first.
	methods := aMethods.
	class := aClass
]
