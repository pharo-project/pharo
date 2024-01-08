Class {
	#name : 'ReBrowseClassReferencesChoice',
	#superclass : 'ReRemoveClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'accessing' }
ReBrowseClassReferencesChoice >> action [

	driver browseReferences
]

{ #category : 'accessing' }
ReBrowseClassReferencesChoice >> description [

	^ 'Browse class references'
]
