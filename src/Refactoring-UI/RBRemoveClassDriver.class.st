Class {
	#name : #RBRemoveClassDriver,
	#superclass : #RBDriver,
	#instVars : [
		'refactoring',
		'classes',
		'users'
	],
	#category : #'Refactoring-UI-Drivers'
}

{ #category : #'as yet unclassified' }
RBRemoveClassDriver >> runRefactoring [

	| select |
	self configureRefactoring.
	[
	[
	users := refactoring senders.

	users size = 0
		ifTrue: [ self openPreviewWithChanges: refactoring generateChanges ]
		ifFalse: [ 
		
		select := SpSelectDialog new 
			title: 'There are ', users size asString,  ' classes using calling ', refactoring classNames;
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

{ #category : #'as yet unclassified' }
RBRemoveClassDriver >> scopes: refactoringScopes classes: aColclasses [
	
	scopes := refactoringScopes.
	classes := aColclasses
]
