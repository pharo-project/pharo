"
I am a public announcement sent when a new Package is renamed
"
Class {
	#name : 'PackageRenamed',
	#superclass : 'PackageAnnouncement',
	#instVars : [
		'oldName',
		'newName'
	],
	#category : 'System-Announcements-System-Packages',
	#package : 'System-Announcements',
	#tag : 'System-Packages'
}

{ #category : 'instance creation' }
PackageRenamed class >> to: aPackage oldName: aSymbol newName: anotherSymbol [

	^ (self to: aPackage)
		  oldName: aSymbol;
		  newName: anotherSymbol;
		  yourself
]

{ #category : 'accessing' }
PackageRenamed >> newName [

	^ newName
]

{ #category : 'accessing' }
PackageRenamed >> newName: anObject [

	newName := anObject
]

{ #category : 'accessing' }
PackageRenamed >> oldName [

	^ oldName
]

{ #category : 'accessing' }
PackageRenamed >> oldName: anObject [

	oldName := anObject
]
