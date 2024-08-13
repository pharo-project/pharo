"
Abstract class that groups behavior related to method generation.
"
Class {
	#name : 'ReGenerateMethodDriver',
	#superclass : 'RBInteractionDriver',
	#instVars : [
		'targetClass',
		'selectedVariables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
ReGenerateMethodDriver >> changes [
	"Remember should not call generateChanges"

	^ refactoring privateTransform; changes


]

{ #category : 'execution' }
ReGenerateMethodDriver >> configureRefactoring [

	refactoring := self refactoringClass 
		className: self targetClass name
		variables: selectedVariables
]

{ #category : 'configuration' }
ReGenerateMethodDriver >> defaultSelectDialog [

	| dialog |
	dialog := SpSelectMultipleDialog new.
	dialog
	  	title: self dialogTitle;
	  	label: 'Please select the variable(s)';
	  	items: self targetClass instanceVariableNames;
		display: [ :each | each asString ];
		displayIcon: [ :each | self iconNamed: each systemIconName ];
	  	openModal.
	^ dialog
]

{ #category : 'configuration' }
ReGenerateMethodDriver >> dialogTitle [

	self subclassResponsibility 
]

{ #category : 'execution' }
ReGenerateMethodDriver >> refactoringClass [ 

	^ self class refactoringClass
]

{ #category : 'execution' }
ReGenerateMethodDriver >> runRefactoring [

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
