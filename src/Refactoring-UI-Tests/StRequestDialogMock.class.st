Class {
	#name : 'StRequestDialogMock',
	#superclass : 'Object',
	#instVars : [
		'response'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'accessing' }
StRequestDialogMock >> message: aString [

	^ self
]

{ #category : 'api - events' }
StRequestDialogMock >> onCancel: aBlock [

	^ self
]

{ #category : 'api - showing' }
StRequestDialogMock >> openModal [

	^ response
]

{ #category : 'accessing' }
StRequestDialogMock >> response: aString [
	response := aString
]

{ #category : 'accessing' }
StRequestDialogMock >> text: aString [ 
	^ self
]

{ #category : 'accessing' }
StRequestDialogMock >> title: aString [ 
	^ self
]

{ #category : 'api' }
StRequestDialogMock >> validateAnswer: aBlock [

	^ self
]
