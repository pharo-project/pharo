"
Interaction object responsible for driving the analysis and interactive generation of equal and hash methods.

The driver could be instantiated by providing a target class (`targetClass:`), or interactively using the select dialog.
It uses the `RBGenerateEqualHashTransformation`.
"
Class {
	#name : 'ReGenerateEqualAndHashDriver',
	#superclass : 'ReGenerateMethodDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReGenerateEqualAndHashDriver class >> driverName [

	^ 'Generate equal and hash'
]

{ #category : 'accessing' }
ReGenerateEqualAndHashDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'configuration' }
ReGenerateEqualAndHashDriver >> dialogTitle [

	^ 'Generate equal and hash methods'
]

{ #category : 'execution' }
ReGenerateEqualAndHashDriver >> refactoringClass [

	^ ReGenerateEqualHashTransformation
]
