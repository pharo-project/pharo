Class {
	#name : 'ReRenameAndBrowseMethodOverridesChoice',
	#superclass : 'ReMethodChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReRenameAndBrowseMethodOverridesChoice >> action [

	driver renameMethod.
	driver browseOverrides
]

{ #category : 'accessing' }
ReRenameAndBrowseMethodOverridesChoice >> description [

	^ 'Rename method and browse existing methods that will then be overriden'
]
