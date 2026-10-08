Class {
	#name : 'StInformDialogMock',
	#superclass : 'Object',
	#instVars : [
		'shownMessage'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'accessing' }
StInformDialogMock >> label: aString [
	"Spec is migrating its API from #label: to message:. Delete me once all users will be updated."

	^ self message: aString
]

{ #category : 'accessing' }
StInformDialogMock >> message: aString [

	shownMessage := aString
]

{ #category : 'api - showing' }
StInformDialogMock >> openModal [
]

{ #category : 'accessing' }
StInformDialogMock >> shownMessage [

	^ shownMessage
]

{ #category : 'accessing' }
StInformDialogMock >> text: aString [

	^ 'nothing'
]

{ #category : 'accessing' }
StInformDialogMock >> title: aString [

	^ 'nothing'
]
