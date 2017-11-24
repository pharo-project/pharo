"
A GTSpotterRecorderDiveOutEvent  models the action of  diving out a step.

"
Class {
	#name : #GTSpotterRecorderDiveOutEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderDiveOutEvent >> accept: aVisitor [
	^ aVisitor visitDiveOutEvent: self
]

{ #category : #accessing }
GTSpotterRecorderDiveOutEvent >> elementName [
	^ self infoAt: #elementName
]

{ #category : #accessing }
GTSpotterRecorderDiveOutEvent >> elementType [
	^ self infoAt: #elementType
]

{ #category : #initialization }
GTSpotterRecorderDiveOutEvent >> fromAnnouncement: anAnnouncement [
	super fromAnnouncement: anAnnouncement.
	self infoAt: #elementName putHashForText: anAnnouncement element asString.
	self infoAt: #elementType putHashForText: anAnnouncement element class name asString.
]

{ #category : #testing }
GTSpotterRecorderDiveOutEvent >> isDiveOut [
	^ true
]
