"
the annoucement will be emitted when removing a class or a trait using:  
	=> removeFromSystem
"
Class {
	#name : #ClassRemoved,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'categoryName',
		'classRemoved',
		'ownerPackage'
	],
	#category : 'System-Announcements-System-Classes'
}

{ #category : #'instance creation' }
ClassRemoved class >> class: aClass category: aCategoryName [
	^self new
			classRemoved: aClass;
			categoryName: aCategoryName;
			yourself.
]

{ #category : #accessing }
ClassRemoved >> categoryName [
	
	^ categoryName
]

{ #category : #accessing }
ClassRemoved >> categoryName: anObject [
	
	categoryName := anObject
]

{ #category : #accessing }
ClassRemoved >> classAffected [
	^self classRemoved
]

{ #category : #accessing }
ClassRemoved >> classRemoved [
	
	^ classRemoved
]

{ #category : #accessing }
ClassRemoved >> classRemoved: aClass [
	
	classRemoved := aClass.
	ownerPackage := aClass package
]

{ #category : #accessing }
ClassRemoved >> classTagAffected [
	^ownerPackage toTagName: categoryName 
]

{ #category : #accessing }
ClassRemoved >> packageAffected [
	^ownerPackage
]
