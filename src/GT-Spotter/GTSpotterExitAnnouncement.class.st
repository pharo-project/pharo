Class {
	#name : #GTSpotterExitAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'eventType'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterExitAnnouncement >> eventType [
	^ eventType
]

{ #category : #accessing }
GTSpotterExitAnnouncement >> eventType: anObject [
	eventType := anObject
]
