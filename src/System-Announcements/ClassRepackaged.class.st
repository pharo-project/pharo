"
Notify about repackaging of a class
"
Class {
	#name : 'ClassRepackaged',
	#superclass : 'ClassAnnouncement',
	#instVars : [
		'classRepackaged',
		'oldTag',
		'newTag'
	],
	#category : 'System-Announcements-System-Classes',
	#package : 'System-Announcements',
	#tag : 'System-Classes'
}

{ #category : 'instance creation' }
ClassRepackaged class >> classRepackaged: aClass oldTag: oldTag newTag: newTag [

	^ self new
		  classRepackaged: aClass;
		  oldTag: oldTag;
		  newTag: newTag;
		  yourself
]

{ #category : 'accessing' }
ClassRepackaged >> classAffected [

	^ classRepackaged
]

{ #category : 'accessing' }
ClassRepackaged >> classRepackaged [

	^ classRepackaged
]

{ #category : 'accessing' }
ClassRepackaged >> classRepackaged: anObject [

	classRepackaged := anObject
]

{ #category : 'accessing' }
ClassRepackaged >> newPackage [

	^ self newTag package
]

{ #category : 'accessing' }
ClassRepackaged >> newTag [

	^ newTag
]

{ #category : 'accessing' }
ClassRepackaged >> newTag: anObject [

	newTag := anObject
]

{ #category : 'accessing' }
ClassRepackaged >> oldPackage [

	^ self oldTag package
]

{ #category : 'accessing' }
ClassRepackaged >> oldTag [

	^ oldTag
]

{ #category : 'accessing' }
ClassRepackaged >> oldTag: anObject [

	oldTag := anObject
]

{ #category : 'accessing' }
ClassRepackaged >> packageTagsAffected [

	^ {
		  self newTag.
		  self oldTag }
]

{ #category : 'testing' }
ClassRepackaged >> packagedChanged [

	^ self oldPackage ~= self newPackage
]

{ #category : 'accessing' }
ClassRepackaged >> packagesAffected [

	^ { self oldPackage. self newPackage }
]
