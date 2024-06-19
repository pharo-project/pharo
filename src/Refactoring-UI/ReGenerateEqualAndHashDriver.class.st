"
Interaction object responsible for driving the analysis and interactive generation of equal and hash methods.

The driver could be instantiated by providing a target class (`targetClass:`), or interactively using the select dialog.
It uses the `RBGenerateEqualHashTransformation`.
"
Class {
	#name : 'ReGenerateEqualAndHashDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'targetClass',
		'selectedVariables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'factory method' }
ReGenerateEqualAndHashDriver class >> refactoringClass [

	^ ReGenerateEqualHashTransformation
]

{ #category : 'execution' }
ReGenerateEqualAndHashDriver >> changes [
	"Remember should not call generateChanges"

	^ refactoring privateTransform; changes


]

{ #category : 'execution' }
ReGenerateEqualAndHashDriver >> configureRefactoring [

	refactoring := self refactoringClass 
		className: self targetClass name
		variables: selectedVariables
]

{ #category : 'configuration' }
ReGenerateEqualAndHashDriver >> defaultSelectDialog [

	| dialog |
	dialog := SpSelectMultipleDialog new.
	dialog
	  	title: 'Generate equal and hash methods';
	  	label: 'Please select the variable(s)';
	  	items: self targetClass instanceVariableNames;
		display: [ :each | each asString ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
	  	openModal.
	^ dialog
]

{ #category : 'execution' }
ReGenerateEqualAndHashDriver >> refactoringClass [ 

	^ self class refactoringClass
]

{ #category : 'execution' }
ReGenerateEqualAndHashDriver >> runRefactoring [

	| changes |
	
	self selectDialog
		ifNil: [ ^ self ]
		ifNotNil: [ selectedVariables := self selectDialog selectedItems ].
	selectedVariables ifEmpty: [ ^ self ].

	self configureRefactoring.
	changes := refactoring generateChanges.
	self openPreviewWithChanges: changes 


]

{ #category : 'accessing' }
ReGenerateEqualAndHashDriver >> scopes: refactoringScopes [

	scopes := refactoringScopes.
	model := self refactoringScopeOn: scopes last.
	targetClass := model classFor: model environment classes anyOne.
]

{ #category : 'accessing' }
ReGenerateEqualAndHashDriver >> targetClass [

	^ targetClass
]

{ #category : 'accessing' }
ReGenerateEqualAndHashDriver >> targetClass: anObject [

	targetClass := anObject
]
