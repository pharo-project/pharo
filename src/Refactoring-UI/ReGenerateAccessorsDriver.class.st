"
Interaction object responsible for driving the analysis and interactive generation of equal and hash methods.

The driver could be instantiated by providing a target class (`targetClass:`), or interactively using the select dialog.
It uses the `RBGenerateEqualHashTransformation`.
"
Class {
	#name : 'ReGenerateAccessorsDriver',
	#superclass : 'ReGenerateMethodDriver',
	#instVars : [
		'scopeInstanceSide'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReGenerateAccessorsDriver class >> driverName [

	^ 'Generate accessors'
]

{ #category : 'configuration' }
ReGenerateAccessorsDriver >> configureRefactoring [

	refactoring := ReUpFrontPreconditionCheckingCompositeRefactoring new
		               model: model;
		               refactorings: (selectedVariables collect: [ :eachSlot | self refactoringForSlot: eachSlot ]);
		               yourself
]

{ #category : 'execution' }
ReGenerateAccessorsDriver >> refactoringClass [

	^ ReCreateAccessorsForVariableTransformation
]

{ #category : 'initialization' }
ReGenerateAccessorsDriver >> refactoringForSlot: aSlot [

	^ scopeInstanceSide
		  ifFalse: [ self refactoringClass model: model classVariable: aSlot class: self targetClass name ]
		  ifTrue: [ self refactoringClass model: model instanceVariable: aSlot class: self targetClass name ]
]

{ #category : 'configuration' }
ReGenerateAccessorsDriver >> runRefactoring [

	self configureRefactoring.
	self applyChanges
]

{ #category : 'accessing' }
ReGenerateAccessorsDriver >> scopeInstanceSide: aBoolean [

	scopeInstanceSide := aBoolean
]

{ #category : 'configuration' }
ReGenerateAccessorsDriver >> selectedVariables: aCollection [

	selectedVariables := aCollection
]
