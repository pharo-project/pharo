Class {
	#name : 'RBRemoveClassDriver',
	#superclass : 'RBDriver',
	#instVars : [
		'classes',
		'users'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'resources' }
RBRemoveClassDriver >> configureRefactoring [

	refactoring := RBRemoveClassRefactoring
		               model: model
		               classNames: (classes collect: [ :cl | cl name ])
]

{ #category : 'execution' }
RBRemoveClassDriver >> runRefactoring [

	| select |
	self configureRefactoring.
	[
	[
	users := refactoring senders.

	users
		ifEmpty: [ self openPreviewWithChanges: refactoring generateChanges ]
		ifNotEmpty: [ 
		
		select := SpSelectDialog new 
			title: 'There are ', users size asString,  ' classes using calling ', refactoring classNames asString;
			label: 'Select a strategy';
			items: (RBRemoveClassChoice subclasses collect: [ :each | each  new driver: self]);
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

{ #category : 'as yet unclassified' }
RBRemoveClassDriver >> scopes: refactoringScopes classes: aColclasses [
	
	scopes := refactoringScopes.
	classes := aColclasses
]
