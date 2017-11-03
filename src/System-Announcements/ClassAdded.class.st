"
This announcement will be emitted when a class or a trait is added, using:
	=> Trait >> named: (the notification is done in Trait >> named:uses:category:env:)
	=> Class >> subclass:
	
"
Class {
	#name : #ClassAdded,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'classAdded',
		'classCategory'
	],
	#category : #System-Announcements-System-Classes
}

{ #category : #'instance creation' }
ClassAdded class >> class: aClass category: aClassCategoryName [
	^self new
			classAdded: aClass;
			classCategory: aClassCategoryName;
			yourself
]

{ #category : #accessing }
ClassAdded >> classAdded [
	^classAdded
]

{ #category : #accessing }
ClassAdded >> classAdded: aClass [
	classAdded := aClass
]

{ #category : #accessing }
ClassAdded >> classAffected [
	^self classAdded
]

{ #category : #accessing }
ClassAdded >> classCategory [
	^classCategory
]

{ #category : #accessing }
ClassAdded >> classCategory: aClassCategoryName [
	classCategory := aClassCategoryName
]
