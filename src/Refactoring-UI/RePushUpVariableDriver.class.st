"
I'm an abstract model for user interaction for the rename variables refactoring.
"
Class {
	#name : 'RePushUpVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variable',
		'isSharedVariable'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'configuration' }
RePushUpVariableDriver >> instantiateRefactoring [

	" we should manage multiple instance variables"
	^ self refactoringClass
       model: model
       variable: variable
       class: class
]

{ #category : 'factory method' }
RePushUpVariableDriver >> refactoringClass [

	^ isSharedVariable
		  ifTrue: [ RePullUpSharedVariableRefactoring ]
		  ifFalse: [ RePullUpInstanceVariableRefactoring ]
]

{ #category : 'execution' }
RePushUpVariableDriver >> runRefactoring [

	self configureRefactoring.
	self applyChanges
]

{ #category : 'factory method' }
RePushUpVariableDriver >> scopes: aCollection variable: varName shared: isSharedVar to: aClass [ 

	"The driver is initialized in interactive mode (i.e. we will ask for the newName to the user while running the refactoring"
	
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first.
	class := aClass.
	variable := varName.
	isSharedVariable := isSharedVar
]
