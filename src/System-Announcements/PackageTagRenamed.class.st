"
I am announced when existing package tag is removed
"
Class {
	#name : #PackageTagRenamed,
	#superclass : #PackageTagAnnouncement,
	#instVars : [
		'newName'
	],
	#category : #'System-Announcements-System-Packages'
}

{ #category : #'instance creation' }
PackageTagRenamed class >> in: aPackage tag: aSymbol to: newTagName [

	^(self in: aPackage tag: aSymbol)
		newName: newTagName
]

{ #category : #'class initialization' }
PackageTagRenamed class >> initialize [

	self deprecatedAliases: { 'ClassTagRenamed' }
]

{ #category : #accessing }
PackageTagRenamed >> newName [
	^ newName
]

{ #category : #accessing }
PackageTagRenamed >> newName: anObject [
	newName := anObject
]
