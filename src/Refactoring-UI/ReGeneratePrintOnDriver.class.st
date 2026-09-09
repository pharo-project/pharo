"
Interaction object responsible for driving the analysis and interactive generation of the printOn: method.
The driver could be instantiated by providing a target class (`targetClass:`), or interactively using the select dialog.
It uses the `ReGeneratePrintOnTransformation`.
"
Class {
	#name : 'ReGeneratePrintOnDriver',
	#superclass : 'ReGenerateMethodDriver',
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReGeneratePrintOnDriver class >> driverName [

	^ 'Generate printOn:'
]

{ #category : 'accessing' }
ReGeneratePrintOnDriver class >> shouldPreviewChangesByDefault [

	^ false
]

{ #category : 'configuration' }
ReGeneratePrintOnDriver >> dialogTitle [ 

	^ 'Generate printOn: method'
]

{ #category : 'execution' }
ReGeneratePrintOnDriver >> refactoringClass [

	^ ReGeneratePrintOnTransformation 
]
