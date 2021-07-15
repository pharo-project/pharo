"
A GLMBrickAnnouncement is default announcement used for any purpose by bricks.

Instance Variables
	data:		<Object>

data
	- xxxxx

"
Class {
	#name : #GLMBrickAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'data'
	],
	#category : #'Glamour-Morphic-Brick-Announcements'
}

{ #category : #testing }
GLMBrickAnnouncement class >> isDeprecated [
	^ true
]

{ #category : #accessing }
GLMBrickAnnouncement >> data [
	^ data
]

{ #category : #accessing }
GLMBrickAnnouncement >> data: anObject [
	data := anObject
]
