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

{ #category : 'execution' }
ReGenerateAccessorsDriver >> gatherUserInput [

	(refactoring refactorings allSatisfy: [ :each | each findGetterMethod notNil and: [ each findSetterMethod notNil ] ]) ifTrue: [
			InformativeNotification signal: 'Accessors already exist for selected variables.'.
			^ false ].
	^ true
]

{ #category : 'configuration' }
ReGenerateAccessorsDriver >> instantiateRefactoring [

	^ ReUpFrontPreconditionCheckingCompositeRefactoring new
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

{ #category : 'accessing' }
ReGenerateAccessorsDriver >> scopeInstanceSide: aBoolean [

	scopeInstanceSide := aBoolean
]

{ #category : 'initialization' }
ReGenerateAccessorsDriver >> scopes: scopesList scopeInstanceSide: aBoolean selectedVariables: aCollection [

	scopeInstanceSide := aBoolean.
	selectedVariables := aCollection.
	scopes := scopesList.
	model := self refactoringScopeOn: scopesList first
]
