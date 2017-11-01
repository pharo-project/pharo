"
I am raised when a keystroke is done
"
Class {
	#name : #RubKeystroke,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'event'
	],
	#category : 'Rubric-Editing-Announcements'
}

{ #category : #'instance creation' }
RubKeystroke class >> with: anEvent [
	^ self new event: anEvent
]

{ #category : #accessing }
RubKeystroke >> event [
	^ event
]

{ #category : #accessing }
RubKeystroke >> event: anObject [
	event := anObject
]
