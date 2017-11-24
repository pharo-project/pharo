"
A KMPartialMatch is an announcement raised when a partial match is completed
"
Class {
	#name : #KMPartialMatch,
	#superclass : #Announcement,
	#instVars : [
		'event',
		'source'
	],
	#category : #'Keymapping-Core-Announcements'
}

{ #category : #'instance creation' }
KMPartialMatch class >> event: event from: source [

	^ self new
		event: event;
		source: source;
		yourself
]

{ #category : #accessing }
KMPartialMatch >> event [
	^ event
]

{ #category : #accessing }
KMPartialMatch >> event: anObject [
	event := anObject
]

{ #category : #accessing }
KMPartialMatch >> source [
	^ source
]

{ #category : #accessing }
KMPartialMatch >> source: anObject [
	source := anObject
]
