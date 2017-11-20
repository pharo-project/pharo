"
I am a public announcement sent when a new RPackage is renamed
"
Class {
	#name : #RPackageRenamed,
	#superclass : #RPackageAnnouncement,
	#instVars : [
		'oldName',
		'newName'
	],
	#category : #RPackage-Core
}

{ #category : #'instance creation' }
RPackageRenamed class >> to: aPackage oldName: aSymbol newName: anotherSymbol [

	^ (self to: aPackage) 
		oldName: aSymbol;
		newName: anotherSymbol;
		yourself
]

{ #category : #accessing }
RPackageRenamed >> newName [

	^ newName
]

{ #category : #accessing }
RPackageRenamed >> newName: anObject [

	newName := anObject
]

{ #category : #accessing }
RPackageRenamed >> oldName [

	^ oldName
]

{ #category : #accessing }
RPackageRenamed >> oldName: anObject [

	oldName := anObject
]
