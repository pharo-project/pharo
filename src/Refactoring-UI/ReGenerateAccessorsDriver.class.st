"
Interaction object responsible for driving the analysis and interactive generation of equal and hash methods.

The driver could be instantiated by providing a target class (`targetClass:`), or interactively using the select dialog.
It uses the `RBGenerateEqualHashTransformation`.
"
Class {
	#name : 'ReGenerateAccessorsDriver',
	#superclass : 'ReGenerateMethodDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'displaying' }
ReGenerateAccessorsDriver class >> refactoringClass [

	^ ReCreateAccessorsForVariableTransformation
]

{ #category : 'configuration' }
ReGenerateAccessorsDriver >> configureRefactoring [
		
	refactoring := ReUpFrontPreconditionCheckingCompositeRefactoring new
							model: model; 
							refactorings: (selectedVariables collect: [:each | 
									self refactoringClass 
										model: model 
										instanceVariable: each 
										class: self targetClass name ]);
								yourself.
]

{ #category : 'configuration' }
ReGenerateAccessorsDriver >> runRefactoring [

	| changes |
	self configureRefactoring.
	changes := refactoring generateChanges.
	self openPreviewWithChanges: changes 
]

{ #category : 'configuration' }
ReGenerateAccessorsDriver >> selectedVariables: aCollection [

	selectedVariables := aCollection
]
