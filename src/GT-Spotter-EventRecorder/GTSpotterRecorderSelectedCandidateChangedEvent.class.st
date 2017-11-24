"
A GTSpotterRecorderSelectedCandidateChangedEvent models the action of  selecting an element in the search results.

"
Class {
	#name : #GTSpotterRecorderSelectedCandidateChangedEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderSelectedCandidateChangedEvent >> accept: aVisitor [
	^ aVisitor visitSelectedCandidateChangedEvent: self
]

{ #category : #accessing }
GTSpotterRecorderSelectedCandidateChangedEvent >> elementName [
	^ self infoAt: #elementName
]

{ #category : #accessing }
GTSpotterRecorderSelectedCandidateChangedEvent >> elementType [
	^ self infoAt: #elementType
]

{ #category : #accessing }
GTSpotterRecorderSelectedCandidateChangedEvent >> eventType [
	^ self infoAt: #eventType
]

{ #category : #initialization }
GTSpotterRecorderSelectedCandidateChangedEvent >> fromAnnouncement: anAnnouncement [
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.
	self infoAt: #elementName putHashForText: anAnnouncement selected asString.
	self infoAt: #elementType putHashForText: anAnnouncement selected class name asString.

]

{ #category : #testing }
GTSpotterRecorderSelectedCandidateChangedEvent >> isUserSelection [
	^ self eventType notNil and: [ (self eventType = #unknown) not ]
]

{ #category : #testing }
GTSpotterRecorderSelectedCandidateChangedEvent >> isUserSelectionClass [
	^ true
]
