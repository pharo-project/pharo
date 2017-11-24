"
Announcement raised when a boolean value changed
"
Class {
	#name : #NautilusBooleanAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'boolean'
	],
	#category : #'NautilusCommon-Announcements'
}

{ #category : #'instance creation' }
NautilusBooleanAnnouncement class >> boolean: aBoolean [

	^ self new boolean: aBoolean
]

{ #category : #accessing }
NautilusBooleanAnnouncement >> boolean [

		^ boolean
]

{ #category : #accessing }
NautilusBooleanAnnouncement >> boolean: anObject [
	
		boolean := anObject
]
