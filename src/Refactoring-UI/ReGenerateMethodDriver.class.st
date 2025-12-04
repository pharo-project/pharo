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

{ #category : 'execution' }
ReGenerateMethodDriver >> configureRefactoring [

	refactoring := self refactoringClass 
		className: self targetClass name
		variables: selectedVariables
]

{ #category : 'configuration' }
ReGenerateMethodDriver >> dialogTitle [

	self subclassResponsibility 
]

{ #category : 'configuration' }
ReGenerateMethodDriver >> instantiateRefactoring [

	^ self refactoringClass 
		className: self targetClass name
		variables: selectedVariables
]

{ #category : 'execution' }
ReGenerateMethodDriver >> refactoringClass [ 

	^ self class refactoringClass
]

{ #category : 'execution' }
ReGenerateMethodDriver >> runRefactoring [

	| dialog selection |
	"I do not use defaultSelectDialog because it is used mainly for choices selection 
	when breaking changes. Here this is for doing something else. "

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
	selection ifEmpty: [ ^ self ].
	selectedVariables := selection.
	self configureRefactoring.
	self applyChanges.
	^ true
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
