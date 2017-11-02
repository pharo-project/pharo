"
Common superclass for group announcements
"
Class {
	#name : #AbstractGroupAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'group',
		'holder'
	],
	#category : 'Nautilus-GroupManager-Announcements'
}

{ #category : #'instance creation' }
AbstractGroupAnnouncement class >> group: aGroup [

	^ self new 
		group: aGroup;
		yourself
]

{ #category : #'instance creation' }
AbstractGroupAnnouncement class >> group: aGroup from: anHolder [

	^ self new 
		group: aGroup;
		holder: anHolder;
		yourself
]

{ #category : #accessing }
AbstractGroupAnnouncement >> group [

		^ group
]

{ #category : #accessing }
AbstractGroupAnnouncement >> group: anObject [
	
		group := anObject
]

{ #category : #accessing }
AbstractGroupAnnouncement >> holder [

		^ holder
]

{ #category : #accessing }
AbstractGroupAnnouncement >> holder: anObject [
	
		holder := anObject
]
