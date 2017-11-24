"
I am raised when a mouse down is done
"
Class {
	#name : #RubMouseClick,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'event'
	],
	#category : #'Rubric-Editing-Announcements'
}

{ #category : #'instance creation' }
RubMouseClick class >> with: anEvent [
	^ self new event: anEvent
]

{ #category : #accessing }
RubMouseClick >> event [
	^ event
]

{ #category : #accessing }
RubMouseClick >> event: anObject [
	event := anObject
]
