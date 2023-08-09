"
I'm a model for user interaction for the remove method refactoring.
"
Class {
	#name : #RBRemoveMethodDriver,
	#superclass : #RBDriver,
	#instVars : [
		'class',
		'variables',
		'refactoring',
		'methods'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #resources }
RBRemoveMethodDriver >> configureRefactoring [

	refactoring := RBRemoveMethodRefactoring
		               model: model
		               removeMethods: (methods collect: [ :each | each selector ])
		               from: class
]

{ #category : #resources }
RBRemoveMethodDriver >> refactoring [

	refactoring := RBRemoveMethodRefactoring
		               model: model
		               removeMethods: (methods collect: [ :each | each selector ])
		               from: class
]

{ #category : #execution }
RBRemoveMethodDriver >> runRefactoring [

	| select |
	self configureRefactoring.
	[
	[
	| changes senders |
	senders := refactoring senders.

	senders size = 0
		ifTrue: [ changes := refactoring generateChanges ]
		ifFalse: [ 
		
		
		select := SpSelectDialog new 
			title: 'There are ', senders size asString,  ' methods calling ', refactoring selector;
			label: 'Select a strategy';
			items: (RBRemoveMethodChoice subclasses collect: [ :each | each  new refactoring: refactoring]);
		display: [ :each | each description ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
		openModal.
		
	select ifNotNil: [ 
		select inspect ]
				
		].


	self openPreviewWithChanges: { changes } ]
		on: RBApplicabilityChecksFailedError
		do: [ :err | ^ RBRefactoringError signal: err messageText ] ]
		on: RBBreakingChangeChecksFailedWarning
		do: [ :err |
			| dialog |
			dialog := SpConfirmDialog new
				          title: 'Methods are still calling removed method';
				          label: 'Do you want to browse them?';
				          acceptLabel: 'Sure!';
				          cancelLabel: 'No, forget it';
				          openModal.
			dialog ifTrue: [
				self furtherActionFor:
					(RBMethodStillCalled new refactoring: refactoring) ].
			err return ]
]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes method: aMethod for: aClass [
	
	scopes := refactoringScopes.
	variables := {aMethod}.
	class := aClass
]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes methods: aMethods for: aClass [
	
	scopes := refactoringScopes.
	methods := aMethods.
	class := aClass
]

{ #category : #initialization }
RBRemoveMethodDriver >> scopes: refactoringScopes variable: aVariable for: aClass [
	
	scopes := refactoringScopes.
	variables := {aVariable}.
	class := aClass
]
