"
I am an abstract announcement relates to text area bounds changing
"
Class {
	#name : #RubBoundsChanged,
	#superclass : #RubMorphAnnouncement,
	#instVars : [
		'previousBounds'
	],
	#category : #'Rubric-Editing-Announcements'
}

{ #category : #accessing }
RubBoundsChanged >> previousBounds [
	^ previousBounds
]

{ #category : #accessing }
RubBoundsChanged >> previousBounds: anObject [
	previousBounds := anObject
]
