"
I am announced when existing class tag is removed
"
Class {
	#name : #ClassTagRenamed,
	#superclass : #ClassTagAnnouncement,
	#instVars : [
		'newName'
	],
	#category : #'System-Announcements-System-Packages'
}

{ #category : #'instance creation' }
ClassTagRenamed class >> in: aPackage tag: aSymbol to: newTagName [

	^(self in: aPackage tag: aSymbol)
		newName: newTagName 
]

{ #category : #accessing }
ClassTagRenamed >> newName [
	^ newName
]

{ #category : #accessing }
ClassTagRenamed >> newName: anObject [
	newName := anObject
]
