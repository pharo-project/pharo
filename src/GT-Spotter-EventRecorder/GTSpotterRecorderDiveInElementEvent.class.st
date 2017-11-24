"
A GTSpotterRecorderDiveInElementEvent models the action of diving in a selected element.

"
Class {
	#name : #GTSpotterRecorderDiveInElementEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderDiveInElementEvent >> accept: aVisitor [
	^ aVisitor visitDiveInElementEvent: self
]

{ #category : #accessing }
GTSpotterRecorderDiveInElementEvent >> elementName [
	^ self infoAt: #elementName
]

{ #category : #accessing }
GTSpotterRecorderDiveInElementEvent >> elementType [
	^ self infoAt: #elementType
]

{ #category : #accessing }
GTSpotterRecorderDiveInElementEvent >> eventType [
	^ self infoAt: #eventType
]

{ #category : #initialization }
GTSpotterRecorderDiveInElementEvent >> fromAnnouncement: anAnnouncement [
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.
	self infoAt: #elementName putHashForText: anAnnouncement element asString.
	self infoAt: #elementType putHashForText: anAnnouncement element class name asString.

]

{ #category : #testing }
GTSpotterRecorderDiveInElementEvent >> isDiveIn [
	^ true
]
