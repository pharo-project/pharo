"
I'm an abstract model for user interaction for the rename variables refactoring.
"
Class {
	#name : 'RePushUpVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variables'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
RePushUpVariableDriver class >> driverName [

	^ 'Push up variable'
]

{ #category : 'configuration' }
RePushUpVariableDriver >> instantiateRefactoring [

	^ ReCompositeRefactoring new
		  model: model;
		  refactorings: (variables collect: [ :var |
						   | refactoringClass |
						   refactoringClass := var isClassVariable
							                       ifTrue: [ RePullUpSharedVariableRefactoring ]
							                       ifFalse: [ RePullUpInstanceVariableRefactoring ].
						   refactoringClass model: model variable: var name class: class ])
]

{ #category : 'factory method' }
RePushUpVariableDriver >> scopes: aCollection variables: aVariableCollection to: aClass [

	scopes := aCollection.
	model := self refactoringScopeOn: scopes first.
	class := aClass.
	variables := aVariableCollection
]
