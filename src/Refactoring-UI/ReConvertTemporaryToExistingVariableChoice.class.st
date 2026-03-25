Class {
	#name : 'ReConvertTemporaryToExistingVariableChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReConvertTemporaryToExistingVariableChoice >> action [

	driver convertTemporaryToExistingVariable
]

{ #category : 'accessing' }
ReConvertTemporaryToExistingVariableChoice >> description [

	^ 'Migrate it to existing instance variable'
]
