Class {
	#name : 'StInformDialogMock',
	#superclass : 'Object',
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

	^ 'nothing'
]

{ #category : 'api - showing' }
StInformDialogMock >> openModal [
]

{ #category : 'accessing' }
StInformDialogMock >> text: aString [

	^ 'nothing'
]

{ #category : 'accessing' }
StInformDialogMock >> title: aString [

	^ 'nothing'
]
