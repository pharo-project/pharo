"
This class stores the announcements raised by the Glamour model in an ordered collection.

The collection can later be used for debugging.
"
Class {
	#name : #GLMMemoryLogger,
	#superclass : #GLMLogger,
	#instVars : [
		'announcements'
	],
	#category : #'Glamour-Helpers'
}

{ #category : #accessing }
GLMMemoryLogger >> announcements [
	^ announcements
]

{ #category : #accessing }
GLMMemoryLogger >> announcements: anObject [
	announcements := anObject
]

{ #category : #initialization }
GLMMemoryLogger >> initialize [ 
	announcements := OrderedCollection new
]

{ #category : #logging }
GLMMemoryLogger >> logAnnouncement: anAnnouncement from: aGLMObject [ 
	announcements add: anAnnouncement
]
