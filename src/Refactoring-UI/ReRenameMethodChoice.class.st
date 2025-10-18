Class {
	#name : 'ReRenameMethodChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRenameMethodChoice >> action [

	driver renameMethod
]

{ #category : 'accessing' }
ReRenameMethodChoice >> description [

	^ 'Rename method'
]
