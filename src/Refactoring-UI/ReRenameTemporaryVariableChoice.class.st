Class {
	#name : 'ReRenameTemporaryVariableChoice',
	#superclass : 'ReVariableChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRenameTemporaryVariableChoice >> action [

	driver applyChanges
]

{ #category : 'accessing' }
ReRenameTemporaryVariableChoice >> description [

	^ 'Rename temporary variable anyway'
]
