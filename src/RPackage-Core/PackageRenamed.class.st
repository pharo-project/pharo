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
	#category : 'RPackage-Core-Announcements',
	#package : 'RPackage-Core',
	#tag : 'Announcements'
}

{ #category : 'class initialization' }
PackageRenamed class >> initialize [

	self deprecatedAliases: { #RPackageRenamed }
]

{ #category : 'instance creation' }
PackageRenamed class >> to: aPackage oldName: aSymbol newName: anotherSymbol [

	^ (self to: aPackage)
		  oldName: aSymbol;
		  newName: anotherSymbol;
		  yourself
]

{ #category : 'testing' }
PackageRenamed >> affectsClass: aClass [
	^package == aClass package
]

{ #category : 'testing' }
PackageRenamed >> affectsMethod: aMethod [
	^package == aMethod package
]

{ #category : 'testing' }
PackageRenamed >> affectsMethods [
	"methods belong to package which could be represented by tools as part of method"
	^true
]

{ #category : 'testing' }
PackageRenamed >> affectsMethodsDefinedInClass: aClass [

	^ (package includesClass: aClass) or: [ package extendsClass: aClass ]
]

{ #category : 'testing' }
PackageRenamed >> affectsMethodsDefinedInPackage: aPackage [

	^package == aPackage
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
