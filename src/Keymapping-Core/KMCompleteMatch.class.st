"
I am an announcement raised when there is a full match between the key buffer and a shortcut.
"
Class {
	#name : #KMCompleteMatch,
	#superclass : #Announcement,
	#instVars : [
		'event',
		'source'
	],
	#category : #Keymapping-Core-Announcements
}

{ #category : #'instance creation' }
KMCompleteMatch class >> event: event from: source [

	^ self new
		event: event;
		source: source;
		yourself
]

{ #category : #accessing }
KMCompleteMatch >> event [
	^ event
]

{ #category : #accessing }
KMCompleteMatch >> event: anObject [
	event := anObject
]

{ #category : #accessing }
KMCompleteMatch >> source [
	^ source
]

{ #category : #accessing }
KMCompleteMatch >> source: anObject [
	source := anObject
]
