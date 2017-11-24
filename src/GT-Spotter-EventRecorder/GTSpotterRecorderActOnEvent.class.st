"
A GTSpotterRecorderActOnEvent models the action of pressing enter (or double clicking) on an element.

"
Class {
	#name : #GTSpotterRecorderActOnEvent,
	#superclass : #GTSpotterRecorderEvent,
	#category : #'GT-Spotter-EventRecorder-Events'
}

{ #category : #visitor }
GTSpotterRecorderActOnEvent >> accept: aVisitor [
	^ aVisitor visitActOnEvent: self
]

{ #category : #accessing }
GTSpotterRecorderActOnEvent >> closeActionSymbol [
	^ #actOn
]

{ #category : #accessing }
GTSpotterRecorderActOnEvent >> elementName [
	^ self infoAt: #elementName
]

{ #category : #accessing }
GTSpotterRecorderActOnEvent >> elementType [
	^ self infoAt: #elementType
]

{ #category : #accessing }
GTSpotterRecorderActOnEvent >> eventType [
	^ self infoAt: #eventType
]

{ #category : #initialization }
GTSpotterRecorderActOnEvent >> fromAnnouncement: anAnnouncement [
	super fromAnnouncement: anAnnouncement.
	self infoAt: #eventType put: anAnnouncement eventType.
	self infoAt: #elementName putHashForText: anAnnouncement element asString.
	self infoAt: #elementType putHashForText: anAnnouncement element class name asString.
]

{ #category : #testing }
GTSpotterRecorderActOnEvent >> isCloseSpotter [
	^ true
]
