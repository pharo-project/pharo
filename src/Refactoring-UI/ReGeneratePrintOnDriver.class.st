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

{ #category : 'displaying' }
ReGeneratePrintOnDriver class >> refactoringClass [

	^ ReGeneratePrintOnTransformation 
]

{ #category : 'configuration' }
ReGeneratePrintOnDriver >> dialogTitle [ 

	^ 'Generate printOn: method'
]
