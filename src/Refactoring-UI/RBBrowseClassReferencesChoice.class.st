Class {
	#name : 'RBBrowseClassReferencesChoice',
	#superclass : 'RBRemoveClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
RBBrowseClassReferencesChoice >> action [

	driver browseReferences
]

{ #category : 'accessing' }
RBBrowseClassReferencesChoice >> description [

	^ 'Browse class references'
]
