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
	#category : #'RPackage-Core-Announcements'
}

{ #category : #'instance creation' }
RPackageRenamed class >> to: aPackage oldName: aSymbol newName: anotherSymbol [

	^ (self to: aPackage) 
		oldName: aSymbol;
		newName: anotherSymbol;
		yourself
]

{ #category : #testing }
RPackageRenamed >> affectsClass: aClass [
	^package == aClass package 
]

{ #category : #testing }
RPackageRenamed >> affectsMethod: aMethod [
	^package == aMethod package
]

{ #category : #testing }
RPackageRenamed >> affectsMethods [
	"methods belong to package which could be represented by tools as part of method"
	^true
]

{ #category : #testing }
RPackageRenamed >> affectsMethodsDefinedInClass: aClass [

	^(package definesClass: aClass)
		or: [ package extendsClass: aClass ]
]

{ #category : #testing }
RPackageRenamed >> affectsMethodsDefinedInPackage: aPackage [

	^package == aPackage
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
