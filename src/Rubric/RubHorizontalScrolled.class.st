"
I am raised when a horizontal scroll is done
"
Class {
	#name : #RubHorizontalScrolled,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'scrollValue'
	],
	#category : 'Rubric-Editing-Announcements'
}

{ #category : #'instance creation' }
RubHorizontalScrolled class >> scrollValue: aScrollValue [
	^ self new scrollValue: aScrollValue
]

{ #category : #accessing }
RubHorizontalScrolled >> scrollValue [
	^ scrollValue
]

{ #category : #accessing }
RubHorizontalScrolled >> scrollValue: anObject [
	scrollValue := anObject
]
