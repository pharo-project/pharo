"
A GTSpotterRecorderExitAnnouncementEvent  models the action of  exiting Spotter. 

Instance Variables
	timestamp:		<Object>

timestamp
	- xxxxx

"
Class {
	#name : #GTSpotterRecorderExitAnnouncementEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderExitAnnouncementEvent >> accept: aVisitor [
	^ aVisitor visitExitEvent: self
]

{ #category : #accessing }
GTSpotterRecorderExitAnnouncementEvent >> closeActionSymbol [
	^ self eventType
]

{ #category : #accessing }
GTSpotterRecorderExitAnnouncementEvent >> eventType [
	^ self infoAt: #eventType ifAbsent: [ #missing ]
]

{ #category : #initialization }
GTSpotterRecorderExitAnnouncementEvent >> fromAnnouncement: anAnnouncement [
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.


]

{ #category : #testing }
GTSpotterRecorderExitAnnouncementEvent >> isCloseSpotter [
	^ true
]
