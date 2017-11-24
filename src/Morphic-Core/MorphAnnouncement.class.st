"
I am an abstract announcement for morphic events.  I carry the morph where the event has ocurred.

My subclasses should add information for more precise events.
"
Class {
	#name : #MorphAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'morph'
	],
	#category : #'Morphic-Core-Announcements'
}

{ #category : #'instance creation' }
MorphAnnouncement class >> morph: aMorph [
	^self new
			morph: aMorph;
			yourself
]

{ #category : #accessing }
MorphAnnouncement >> morph [
	^ morph
]

{ #category : #accessing }
MorphAnnouncement >> morph: anObject [
	morph := anObject
]
