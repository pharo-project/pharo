"
A NautilusKeyPressed is raised each time a key is pressed on a Nautilus ui
"
Class {
	#name : #NautilusKeyPressed,
	#superclass : #Announcement,
	#instVars : [
		'key'
	],
	#category : #NautilusCommon-Announcements
}

{ #category : #'as yet unclassified' }
NautilusKeyPressed class >> key: aKey [

	^ self new key: aKey
]

{ #category : #accessing }
NautilusKeyPressed >> key [

		^ key
]

{ #category : #accessing }
NautilusKeyPressed >> key: anObject [
	
		key := anObject
]
