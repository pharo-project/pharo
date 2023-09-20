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
		               classNames: (classes collect: [ :cl | cl name ]).
	refactoring prepareForInteractiveMode
]

{ #category : 'removing' }
RBRemoveClassDriver >> removeClassChanges [

	self openPreviewWithChanges: refactoring removeClassChanges

]

{ #category : 'execution' }
RBRemoveClassDriver >> runRefactoring [

	| select haveNoReferences |
	
	self configureRefactoring.
	refactoring applicabilityPreconditions check ifFalse: [
		RBRefactoringError signal: refactoring applicabilityPreconditions errorString ].
	
	haveNoReferences := refactoring preconditionHaveNoReferences.
	haveNoReferences check

	ifTrue: [ self openPreviewWithChanges: refactoring generateChanges ]
	ifFalse: [ 
		| perClassViolators |
		perClassViolators := haveNoReferences violators.
		users := perClassViolators flatCollect: [ :violators | violators ].
		select := SpSelectDialog new 
			title: 'There are ', users size asString,  ' classes using calling ', refactoring classNames asString;
			label: 'Select a strategy';
			items: (RBRemoveClassChoice subclasses collect: [ :each | each  new driver: self]);
			display: [ :each | each description ];
			displayIcon: [ :each | self iconNamed: each systemIconName ];
			openModal.
		
		select 
			ifNotNil: [ 
				select action ]]
	

]

{ #category : 'initialization' }
RBRemoveClassDriver >> scopes: refactoringScopes classes: aColclasses [
	
	scopes := refactoringScopes.
	classes := aColclasses
]
