Class {
	#name : #GTSpotterActOn,
	#superclass : #Announcement,
	#instVars : [
		'element',
		'eventType'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #accessing }
GTSpotterActOn >> element [
	^ element
]

{ #category : #accessing }
GTSpotterActOn >> element: anObject [
	element := anObject
]

{ #category : #accessing }
GTSpotterActOn >> eventType [
	^ eventType
]

{ #category : #accessing }
GTSpotterActOn >> eventType: anObject [
	eventType := anObject
]
