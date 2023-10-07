"
I am announced when existing package tag is removed
"
Class {
	#name : 'PackageTagRenamed',
	#superclass : 'PackageTagAnnouncement',
	#instVars : [
		'oldName',
		'newName'
	],
	#category : 'System-Announcements-System-Packages',
	#package : 'System-Announcements',
	#tag : 'System-Packages'
}

{ #category : 'class initialization' }
PackageTagRenamed class >> initialize [

	self deprecatedAliases: { #ClassTagRenamed . #CategoryRenamed }
]

{ #category : 'instance creation' }
PackageTagRenamed class >> to: aPackageTag oldName: aSymbol newName: anotherSymbol [

	^ (self to: aPackageTag)
		  oldName: aSymbol;
		  newName: anotherSymbol;
		  yourself
]

{ #category : 'accessing' }
PackageTagRenamed >> newName [
	^ newName
]

{ #category : 'accessing' }
PackageTagRenamed >> newName: anObject [
	newName := anObject
]

{ #category : 'accessing' }
PackageTagRenamed >> oldName [

	^ oldName
]

{ #category : 'accessing' }
PackageTagRenamed >> oldName: anObject [

	oldName := anObject
]
