Class {
	#name : #GLMTestAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'parameter'
	],
	#category : 'Glamour-Tests-Core'
}

{ #category : #accessing }
GLMTestAnnouncement >> parameter [
	^ parameter
]

{ #category : #accessing }
GLMTestAnnouncement >> parameter: anObject [
	parameter := anObject
]
