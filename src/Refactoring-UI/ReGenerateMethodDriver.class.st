"
Abstract class that groups behavior related to method generation.
"
Class {
	#name : 'ReGenerateMethodDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'targetClass',
		'selectedVariables'
	],
	#classVars : [
		'DefaultSelectDialog'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'configuration' }
ReGenerateMethodDriver class >> defaultSelectDialog [

	^ DefaultSelectDialog ifNil: [ SpSelectMultipleDialog new ]
]

{ #category : 'testing' }
ReGenerateMethodDriver class >> isAbstract [

	^ self = ReGenerateMethodDriver
]

{ #category : 'configuration' }
ReGenerateMethodDriver >> dialogTitle [

	self subclassResponsibility 
]

{ #category : 'execution' }
ReGenerateMethodDriver >> gatherUserInput [

	| dialog selection |
	dialog := SpSelectMultipleDialog newApplication: self application.
	dialog
		title: self dialogTitle;
		message: 'Please select the variable(s)';
		items: self targetClass instanceVariableNames;
		display: [ :each | each displayString ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
		yourself.
	selection := dialog openModal.
	selection ifNil: [ ^ false ].
	selection ifEmpty: [ ^ false ].
	selectedVariables := selection.
	self configureRefactoring.
	^ true
]

{ #category : 'configuration' }
ReGenerateMethodDriver >> instantiateRefactoring [

	^ self refactoringClass 
		className: self targetClass name
		variables: selectedVariables
]

{ #category : 'execution' }
ReGenerateMethodDriver >> refactoringClass [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReGenerateMethodDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	targetClass := model classFor: model environment classes anyOne.
]

{ #category : 'accessing' }
ReGenerateMethodDriver >> targetClass [

	^ targetClass
]

{ #category : 'accessing' }
ReGenerateMethodDriver >> targetClass: anObject [

	targetClass := anObject
]
