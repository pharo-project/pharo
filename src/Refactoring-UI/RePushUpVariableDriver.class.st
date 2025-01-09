"
I'm an abstract model for user interaction for the rename variables refactoring.
"
Class {
	#name : 'RePushUpVariableDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'class',
		'variable'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'execution' }
RePushUpVariableDriver >> configureRefactoring [

	" we should manage multiple instance variables"
	
	refactoring := self refactoringClass
		               model: model
		               variable: variable
		               class: class
]

{ #category : 'factory method' }
RePushUpVariableDriver >> refactoringClass [
	^ RePullUpInstanceVariableRefactoring
]

{ #category : 'execution' }
RePushUpVariableDriver >> runRefactoring [

	self configureRefactoring.
	self applyChanges
]

{ #category : 'initialization' }
RePushUpVariableDriver >> scopes: aCollection variable: aString to: aClass [ 
	"The driver is initialized in interactive mode (i.e. we will ask for the newName to the user while running the refactoring"
	
	scopes := aCollection.
	model :=  self refactoringScopeOn: scopes first.
	class := aClass.
	variable := aString
]
