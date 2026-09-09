Class {
	#name : 'ReRenameClassChoice',
	#superclass : 'ReClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'actions' }
ReRenameClassChoice >> action [
	
	driver renameClass
]

{ #category : 'accessing' }
ReRenameClassChoice >> description [ 

	^ 'Rename class'
]
