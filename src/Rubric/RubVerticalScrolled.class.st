"
I am raised when a vertical scroll is done
"
Class {
	#name : #RubVerticalScrolled,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'scrollValue'
	],
	#category : 'Rubric-Editing-Announcements'
}

{ #category : #'instance creation' }
RubVerticalScrolled class >> scrollValue: aScrollValue [
	^ self new scrollValue: aScrollValue
]

{ #category : #accessing }
RubVerticalScrolled >> scrollValue [
	^ scrollValue
]

{ #category : #accessing }
RubVerticalScrolled >> scrollValue: anObject [
	scrollValue := anObject
]
