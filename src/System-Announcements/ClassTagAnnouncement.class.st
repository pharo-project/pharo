"
My classes represent class tags modifications on package
"
Class {
	#name : #ClassTagAnnouncement,
	#superclass : #SystemAnnouncement,
	#instVars : [
		'package',
		'tag'
	],
	#category : #'System-Announcements-System-Packages'
}

{ #category : #'instance creation' }
ClassTagAnnouncement class >> in: aPackage tag: aSymbol [
	^self new 
		package: aPackage;
		tag: aSymbol
]

{ #category : #accessing }
ClassTagAnnouncement >> package [
	^ package
]

{ #category : #accessing }
ClassTagAnnouncement >> package: anObject [
	package := anObject
]

{ #category : #accessing }
ClassTagAnnouncement >> packagesAffected [
	^{self package}
]

{ #category : #accessing }
ClassTagAnnouncement >> tag [
	^ tag
]

{ #category : #accessing }
ClassTagAnnouncement >> tag: anObject [
	tag := anObject
]
