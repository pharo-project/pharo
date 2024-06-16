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

{ #category : 'execution' }
ReGenerateEqualAndHashDriver >> configureRefactoring [

	refactoring := RBGenerateEqualHashTransformation 
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
ReGenerateEqualAndHashDriver >> handleExistingVariables: anException [

	| dialog |

	dialog := SpConfirmDialog new
		          title: '#= and #hash methods already defined';
		          label: 'Are you sure you want to override it?';
		          acceptLabel: 'Ok';
		          cancelLabel: 'Cancel';
		          openModal.
	dialog ifTrue: [ anException resume ]
]

{ #category : 'execution' }
ReGenerateEqualAndHashDriver >> runRefactoring [

	self selectDialog
		ifNil: [ ^ self ]
		ifNotNil: [ selectedVariables := self selectDialog selectedItems ].
	selectedVariables ifEmpty: [ ^ self ].
	[
			| changes |
			self configureRefactoring.
			changes := refactoring generateChanges.
			self openPreviewWithChanges: changes 
	]
	on: RBRefactoringWarning
	do: [ : ex | self handleExistingVariables: ex ]
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
